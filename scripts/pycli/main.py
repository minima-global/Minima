import datetime
import requests
import click


@click.group()
def cli():
    pass

@cli.command()
@click.option('--show-mobiles/--no-mobiles', default=False, help='Show mobile nodes')
@click.option('--no-summary/--summary', default=False, help='Hide Summary')
@click.option('--full/--hide-full', default=False, help='Show Full Report')
@click.option('--endpoint', help='network data endpoint')
def status(show_mobiles, no_summary, full, endpoint):
    """Checks the status of the network"""

    r = requests.get(endpoint)

    valid_nodes = []
    valid_nodes_p2p = []
    valid_nodes_mobile = []
    latest_update_time = datetime.datetime.fromtimestamp(0)
    top_block_number = None
    block_number_50 = 0
    top_node_address = ''
    current_hash = ''
    previous_hash = ''
    valid_addresses = []
    # Step 1 get latest timestamp
    for node in r.json():
        major, minor, build = node['minima_version'].split('.')
        if (major == '0') and (minor == '100') and (int(build) >= 13):
            valid_addresses.append(node['address'])
            if node['is_mobile'] == 'True':
                valid_nodes_mobile.append(node)
            else:
                if node['is_accepting_connections'] == 'True':
                    valid_nodes_p2p.append(node)
                else:
                    valid_nodes.append(node)
            ts = datetime.datetime.fromisoformat(node['timestamp'].split('.')[0].replace('Z', ''))
            if ts > latest_update_time:
                latest_update_time = ts
                top_block_number = node['top_block_number']

            if '50_current_hash' in node:
                if node['50_block_number'] > block_number_50:
                    block_number_50 = node['50_block_number']
                    current_hash = node['50_current_hash']
                    previous_hash = node['50_last_hash']
            if node['top_block_number'] >= top_block_number:
                top_node_address = node['address']

    valid_nodes = valid_nodes_p2p + valid_nodes + valid_nodes_mobile

    node_status = []
    node_summary = {
        'p2p_okay': 0,
        'p2p_not_okay': 0,
        'pc_okay': 0,
        'pc_not_okay': 0,
        'mobile_okay': 0,
        'mobile_not_okay': 0,
    }
    out_links = 0
    incoming_links = 0
    for node in valid_nodes:
        status = {}
        status['address'] = node['address']
        status['minima_version'] = node['minima_version']
        status['is_mobile'] = node['is_mobile']
        status['has_external_ip'] = node['is_accepting_connections']
        status['num_p2p_links'] = len(node['in_links']) + len(node['out_links'])
        status['total_links'] = len(node['in_links']) + len(node['out_links']) + len(node['not_accepting_conn_links']) + len(node['none_p2p_links'])
        status['in_sync'] = True
        status['is_mobile'] = node['is_mobile']
        out_links += len(node['out_links'])
        incoming_links += len(node['in_links']) + len(node['not_accepting_conn_links']) + len(node['none_p2p_links'])
        ts = datetime.datetime.fromisoformat(node['timestamp'].split('.')[0].replace('Z', ''))
        max_expected_block_difference = max(((latest_update_time - ts) // datetime.timedelta(seconds=25)) + 2, 2)
        is_okay = True
        issues = []
        # Validity Rules
        tip_string = '  '
        if (top_block_number - node['top_block_number']) > max_expected_block_difference:
            status['in_sync'] = False
            is_okay = False
            issues.append(f"Expected min block number: {top_block_number - max_expected_block_difference} actual: {node['top_block_number']}")
        if status['has_external_ip'] == 'True' and status['num_p2p_links'] == 0:
            is_okay = False
            issues.append(f"No P2P Links")
        if status['has_external_ip'] == 'True' and len(node['out_links']) > 5:
            is_okay = False
            issues.append(f"Too Many OutLinks - {len(node['out_links'])} expecting 5")
        if status['total_links'] > 100:
            is_okay = False
            issues.append(f"Warning - more than 100 connections on this node!")
        if status['total_links'] == 0:
            is_okay = False
            issues.append(f"Node is reporting no connections")
        if '50_current_hash' in node:
            if len({previous_hash, current_hash}.intersection({node['50_current_hash'], node['50_last_hash']})) == 0:
                is_okay = False
                issues.append(f"Node is on a different chain - 50th Block Num: {node['50_block_number']}")
        if node['address'] == top_node_address:
            tip_string = '🔺'

        issues_string = 'Issues: ' + ', '.join(issues) if len(issues) != 0 else ''

        status_icon = '✅' if is_okay else '❌'
        in_sync = '♻️' if status['in_sync'] else '❌'
        node_icon = '📱' if status['is_mobile'] == 'True' else '🖥️'
        p2p_node = '🐙' if status['has_external_ip']  == 'True' else '  '
        if (show_mobiles and (status['is_mobile'] == 'True')) or (status['is_mobile'] != 'True'):
            node_status.append(f"\t {status_icon}{tip_string}{node_icon}\t{p2p_node}  {status['address']}\t Version: {status['minima_version']} Connections: {status['total_links']}\t In-Sync: {in_sync}\t {issues_string}")

        if (status['is_mobile'] != 'True') and (status['has_external_ip'] == 'True'):
            if is_okay:
                node_summary['p2p_okay'] += 1
            else:
                node_summary['p2p_not_okay'] += 1
        elif (status['is_mobile'] != 'True') and (status['has_external_ip'] != 'True'):
            if is_okay:
                node_summary['pc_okay'] += 1
            else:
                node_summary['pc_not_okay'] += 1
        elif status['is_mobile'] == 'True':
            if is_okay:
                node_summary['mobile_okay'] += 1
            else:
                node_summary['mobile_not_okay'] += 1

    if not no_summary:
        p2p_string = f"\t 🐙 🖥️\t ✅ {node_summary['p2p_okay']}\t"
        if node_summary['p2p_not_okay'] != 0:
            p2p_string += f" ❌ {node_summary['p2p_not_okay']}"
        pc_string = f"\t    🖥️\t ✅ {node_summary['pc_okay']}\t"
        if node_summary['pc_not_okay'] != 0:
            pc_string += f" ❌ {node_summary['pc_not_okay']}"
        mob_string = f"\t    📱\t ✅ {node_summary['mobile_okay']}\t"
        if node_summary['mobile_not_okay'] != 0:
            mob_string += f" ❌ {node_summary['mobile_not_okay']}"

        click.echo("\t Node Summary")
        click.echo("\t ------------")
        click.echo(f"\t Total In:  {incoming_links}")
        click.echo(f"\t Total Out: {out_links}")
        click.echo("\t ------------")
        click.echo(p2p_string)
        click.echo(pc_string)
        click.echo(mob_string)
        click.echo("\t ------------")
        click.echo(f"\t Total: {len(valid_nodes)}")
        click.echo("")

    if full:
        click.echo("\t Node Status Report")
        click.echo("\t --------------------------------------------------------------------------------------------------------")
        click.echo(f"\t 🐙 Tip Node: {top_node_address} 50th Tip Block Num: {block_number_50} 50th Tip Hash: {current_hash}")
        click.echo("\t --------------------------------------------------------------------------------------------------------")
        for node in node_status:
            click.echo(node)


@cli.command()
@click.option('--endpoint', help='network data endpoint')
def network_map(endpoint):
    r = requests.get(endpoint)
    valid_addresses = []
    valid_nodes = []
    for node in r.json():
        major, minor, build = node['minima_version'].split('.')
        if (major == '0') and (minor == '100') and (int(build) >= 13):
            valid_addresses.append(node['address'])
            valid_nodes.append(node)

    return_data = []
    for node in valid_nodes:
        ret_node = {
            'address': node['address'],
            'out_links': [],
            'in_links': [],
            'client_links': [],
        }
        for link in node['in_links']:
            if link in valid_addresses:
                ret_node['in_links'].append(link)
        for link in node['out_links']:
            if link in valid_addresses:
                ret_node['out_links'].append(link)
        for link in node['not_accepting_conn_links']:
            ret_node['client_links'].append(link)
        for link in node['none_p2p_links']:
            ret_node['client_links'].append(link)
        return_data.append(ret_node)

    print(str(return_data).replace("'", '"'))


if __name__ == '__main__':
    cli()

