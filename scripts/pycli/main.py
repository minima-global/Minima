import datetime
import json
import math
import glob
import time
import requests
import click
import pandas
import plotly.graph_objects as go
import plotly.express as px

LM_OCEAN = 'rgb(244, 248, 255)'
LM_LAND = 'rgb(189, 189, 189)'
LM_LINES = 'rgb(90, 148, 255)'
LM_NODES = 'rgb(90, 189, 255)'
LM_SCALE = [
    'rgb(255, 237, 233)',
    'rgb(255, 220, 213)',
    'rgb(255, 185, 171)',
    'rgb(255, 150, 130)',
    'rgb(255, 115, 88)',
    'rgb(255, 81, 47)',
]
LM_TEXT = 'black'

DM_OCEAN = 'rgb(54, 58, 58)'
DM_LAND = 'rgb(189, 189, 189)'
DM_LINES = 'rgb(90, 148, 255)'
DM_NODES = 'rgb(90, 148, 255)'
DM_SCALE = [
    'rgb(255, 237, 233)',
    'rgb(255, 220, 213)',
    'rgb(255, 185, 171)',
    'rgb(255, 150, 130)',
    'rgb(255, 115, 88)',
    'rgb(255, 81, 47)',
]
DM_TEXT = 'white'


@click.group()
def cli():
    pass


@cli.command()
@click.option('--show-mobiles/--no-mobiles', default=False, help='Show mobile nodes')
@click.option('--no-summary/--summary', default=False, help='Hide Summary')
@click.option('--full/--hide-full', default=False, help='Show Full Report')
@click.option('--failed-only/--all', default=False, help='Show Full Report')
@click.option('--endpoint', help='network data endpoint')
def status(show_mobiles, no_summary, full, endpoint, failed_only):
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
    versions = {}
    # Step 1 get latest timestamp
    for node in r.json():
        major, minor, build = node['minima_version'].split('.')
        if (major == '0') and (minor == '100') and (int(build) >= 13):
            valid_addresses.append(node['address'])
            if node['minima_version'] in versions.keys():
                versions[node['minima_version']] += 1
            else:
                versions[node['minima_version']] = 1
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
        status['total_links'] = len(node['in_links']) + len(node['out_links']) + len(
            node['not_accepting_conn_links']) + len(node['none_p2p_links'])
        status['in_sync'] = True
        status['is_mobile'] = node['is_mobile']
        node_out_links = len(node['out_links'])
        out_links += len(node['out_links'])
        node_in_links = len(node['in_links']) + len(node['not_accepting_conn_links']) + len(node['none_p2p_links'])
        incoming_links += len(node['in_links']) + len(node['not_accepting_conn_links']) + len(node['none_p2p_links'])
        ts = datetime.datetime.fromisoformat(node['timestamp'].split('.')[0].replace('Z', ''))
        max_expected_block_difference = max(((latest_update_time - ts) // datetime.timedelta(seconds=25)) + 10, 2)
        is_okay = True
        issues = []
        # Validity Rules
        tip_string = '  '
        if (top_block_number - node['top_block_number']) > max_expected_block_difference:
            status['in_sync'] = False
            is_okay = False
            issues.append(
                f"Expected min block number: {top_block_number - max_expected_block_difference} actual: {node['top_block_number']}")
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
        if 'nio_inbound' in node.keys():
            if node['nio_inbound'] != node_in_links:
                is_okay = False
                issues.append(f"Different num in links reported. NIO: {node['nio_inbound']} P2P: {node_in_links} Diff: {node['nio_inbound'] - node_in_links}")

            if node['nio_outbound'] != node_out_links:
                is_okay = False
                issues.append(f"Different num out links reported. NIO: {node['nio_outbound']} P2P: {node_out_links} Diff: {node['nio_outbound'] - node_out_links}")

        issues_string = 'Issues: ' + ', '.join(issues) if len(issues) != 0 else ''

        status_icon = '✅' if is_okay else '❌'
        in_sync = '♻️' if status['in_sync'] else '❌'
        node_icon = '📱' if status['is_mobile'] == 'True' else '🖥️'
        p2p_node = '🐙' if status['has_external_ip'] == 'True' else '  '
        padding = ' ' * (20 - len(status['address']))
        if (show_mobiles and (status['is_mobile'] == 'True')) or (status['is_mobile'] != 'True'):
            if not failed_only or (failed_only and not is_okay):
                node_status.append(
                f"\t {status_icon}{tip_string}{node_icon}\t{p2p_node}  {status['address']}{padding}\t Version: {status['minima_version']} Connections: {status['total_links']}\t In-Sync: {in_sync}\t {issues_string}")

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
        click.echo("\t Versions")
        click.echo("\t ------------")
        for key in sorted(versions.keys(), reverse=True):
            click.echo(f"\t Version: {key} Num: {versions[key]}")
        click.echo("\t ------------")
        click.echo(p2p_string)
        click.echo(pc_string)
        click.echo(mob_string)
        click.echo("\t ------------")
        click.echo(f"\t Total: {len(valid_nodes)}")
        click.echo("")

    if full:
        click.echo("\t Node Status Report")
        click.echo(
            "\t --------------------------------------------------------------------------------------------------------")
        click.echo(
            f"\t 🐙 Tip Node: {top_node_address} 50th Tip Block Num: {block_number_50} 50th Tip Hash: {current_hash}")
        click.echo(
            "\t --------------------------------------------------------------------------------------------------------")
        for node in node_status:
            click.echo(node)


def map_ip_to_data(geo_data):
    ip_geo_map = {}
    for node in geo_data:
        ip_geo_map[node['query']] = node
    return ip_geo_map


@cli.command()
@click.option('--endpoint', help='network data endpoint')
def network_map(endpoint):
    r = requests.get(endpoint)


def create_map(data, dt, geo_data):
    valid_addresses = []
    valid_nodes = []
    ip_data_map = map_ip_to_data(geo_data)
    for node in data:
        valid_addresses.append(node['address'].split(':')[0])
        valid_nodes.append(node)
    ips_to_get = set(valid_addresses) - set(ip_data_map.keys())
    new_geo_data = geo_locate_ips(list(ips_to_get))
    geo_data += new_geo_data
    ip_data_map = map_ip_to_data(geo_data)
    df = pandas.DataFrame(geo_data)
    df = df.dropna(subset=['lat', 'lon'])

    def one():
        for node in valid_nodes:
            node_address = node['address'].split(':')[0]
            all_connections = node['in_links'] + node['out_links'] + node['client_links']
            for conn in all_connections:
                conn_address = conn.split(':')[0]
                if conn_address in valid_addresses:
                    if node_address < conn_address:
                        start = node_address
                        end = conn_address
                    else:
                        start = conn_address
                        end = node_address
                    try:
                        yield pandas.DataFrame([
                            {
                                'start_lat': ip_data_map[start]['lat'],
                                'start_lon': ip_data_map[start]['lon'],
                                'end_lat': ip_data_map[end]['lat'],
                                'end_lon': ip_data_map[end]['lon']
                            },
                        ])
                    except Exception:
                        pass

    df_links = pandas.concat(one())

    dfs = pandas.read_csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')

    df_nodes = df.groupby(['lat', 'lon']).first()
    cnt = df.groupby(['lat', 'lon']).count()
    df_nodes = df_nodes.join(cnt[['status']].rename(columns={'status': 'count'}))
    df_nodes['node_size'] = 20 + ((df_nodes['count'] // df_nodes['count'].max()) * 5)
    df_nodes = df_nodes.reset_index()

    country_count = df.groupby('country').count()[['query']]
    df_cnt = df.merge(country_count.rename(columns={'query': 'node_count'}), left_on='country', right_index=True,
                      how='left')
    df_countries = df_cnt.groupby('country').first().reset_index()

    # fig = go.Figure()
    plot(df_countries, df_links, df_nodes, valid_nodes, dt, 'light', True)
    plot(df_countries, df_links, df_nodes, valid_nodes, dt, 'light', False)
    plot(df_countries, df_links, df_nodes, valid_nodes, dt, 'dark', True)
    plot(df_countries, df_links, df_nodes, valid_nodes, dt, 'dark', False)
    return geo_data


def plot(df_countries, df_links, df_nodes, valid_nodes, dt, color_mode, lines):
    if color_mode == 'light':
        OCEAN = LM_OCEAN
        LAND = LM_LAND
        LINES = LM_LINES
        NODES = LM_NODES
        SCALE = LM_SCALE
        TEXT = LM_TEXT
    else:
        OCEAN = DM_OCEAN
        LAND = DM_LAND
        LINES = DM_LINES
        NODES = DM_NODES
        SCALE = DM_SCALE
        TEXT = DM_TEXT

    fig = go.Figure(data=go.Choropleth(
        locations=df_countries['country'],
        locationmode='country names',
        z=df_countries['node_count'],
        zmax=200,
        text=df_countries['country'],
        colorscale=SCALE,
        autocolorscale=False,
        reversescale=False,
        marker_line_color='darkgray',
        marker_line_width=0.5,
        colorbar_title='Nodes',
    ))

    if lines:
        for i, row in df_links.iterrows():
            fig.add_trace(
                go.Scattergeo(
                    # locationmode='USA-states',
                    lon=[row['start_lon'], row['end_lon']],
                    lat=[row['start_lat'], row['end_lat']],
                    mode='lines',
                    line=dict(width=1, color=LINES),
                    opacity=1,
                )
            )

    fig.add_trace(go.Scattergeo(
        # locationmode='USA-states',
        lon=df_nodes['lon'],
        lat=df_nodes['lat'],
        hoverinfo='text',
        text=df_nodes['city'],
        mode='markers',
        marker=dict(
            size=df_nodes['node_size'],
            color=NODES,
            line=dict(
                width=1,
                color='white'
            )
        )))

    fig.add_annotation(x=0.055, y=0.2,
                       text=f"Countries: {len(df_countries)}",
                       showarrow=False,
                       font=dict(
                           family="Courier New, monospace",
                           size=40,
                           color=TEXT
                       ),
                       opacity=1
                       )

    fig.add_annotation(x=0.055, y=0.25,
                       text=f"Node Count: {len(valid_nodes)}",
                       showarrow=False,
                       font=dict(
                           family="Courier New, monospace",
                           size=40,
                           color=TEXT
                       ),
                       opacity=1
                       )

    fig.add_annotation(x=0.055, y=0.3,
                       text=f"Date: {dt.strftime('%d %b %Y')}", #, %H:%M:%S
                       showarrow=False,
                       font=dict(
                           family="Courier New, monospace",
                           size=40,
                           color=TEXT
                       ),
                       opacity=1
                       )
    fig.add_annotation(x=1, y=-0.005,
                       text=f"^ Node locations are approximate",
                       showarrow=False,
                       font=dict(
                           family="Courier New, monospace",
                           size=32,
                       ),
                       opacity=1
                       )

    fig.add_trace(go.Scattergeo(
        lon=[-0.139754],
        lat=[-148.467040],
        text=[f"Node Count: {len(valid_nodes)}"],
        mode='text',
        marker=dict(
            size=20,
            color='rgb(0, 0, 0)',
            line=dict(
                width=3,
                color='rgba(68, 68, 68, 0)'
            ),

        )))

    fig.update_layout(
        # title={
        #     'text': f'Minima Global Node Map',
        #     'font_size': 60,
        #     'x': 0.5,
        #     # 'xanchor': "center"
        # },
        showlegend=False,
        geo=dict(
            # scope='north america',
            # projection_type='azimuthal equal area',
            showland=True,
            landcolor=LAND,
            countrycolor='white',
            showocean=True,
            # oceancolor='rgb(49, 122, 255)',
            oceancolor=OCEAN,
            showcountries=True
        ),
    )

    lines_text = '_plus_lines' if lines else ''
    line_folder = 'lines' if lines else 'no_lines'
    fig.write_image(f"images/{color_mode}/{line_folder}/{dt.strftime('%Y%m%dT%H%M%S')}_{color_mode}{lines_text}.png", width=1920*2, height=1080*2)


def geo_locate_ips(ips):
    # print(f"Getting {len(ips)} ips")
    batches = math.ceil(len(ips) / 100)
    geo_data = []
    for i in range(1, batches + 1):
        try:
            r = requests.post(
                "http://ip-api.com/batch?fields=status,message,country,countryCode,region,regionName,city,lat,lon,query",
                data=str(ips[(i - 1) * 100:(i * 100)]).replace('"', '').replace("'", '"'))
            geo_data += r.json()
        except Exception:
            print("Hit Issue getting ip data, sleeping for 10s")
            time.sleep(10)
    return geo_data


@cli.command()
@click.option('--endpoint', help='network data endpoint')
def maps(endpoint):
    files = sorted(glob.glob('data/*.json'), reverse=True)
    geo_data = []
    for file in files:
        print(file)
        dt = pandas.to_datetime('-'.join(file.split('.')[0].split('-')[2:])).to_pydatetime()
        with open(file) as json_file:
            data = json.load(json_file)
            geo_data = create_map(data, dt, geo_data)


@cli.command()
def video():
    import imageio as iio
    from pygifsicle import optimize

    for color in ['dark', 'light']:
        for lines in ['lines', 'no_lines']:
            images = list()
            files = sorted(glob.glob(f'images/{color}/{lines}/*.png'))
            for file in files:
                im = iio.imread(file)
                images.append(im)

            gif_path = f"images/{color}/minima_network_{lines}.mp4"
            fps = len(images) // 10
            with iio.get_writer(gif_path, mode='I', fps=fps) as writer:
                for i in images:
                    writer.append_data(i)

            # optimize(gif_path, f"images/{color}/minima_network_{lines}_opt.gif")


@cli.command()
@click.option('--endpoint', help='network data endpoint')
def network_health(endpoint):
    import networkx as nx
    import numpy
    G = nx.Graph()


    r = requests.get(endpoint)
    valid_addresses = []
    valid_nodes = []
    for node in r.json():
        major, minor, build = node['minima_version'].split('.')
        if (major == '0') and (minor == '100') and (int(build) >= 13):
            valid_addresses.append(node['address'])
            valid_nodes.append(node)

    G.add_nodes_from(valid_addresses)

    for node in valid_nodes:
        for link in node['out_links']:
            if link in valid_addresses:
                G.add_edge(node['address'], link)

    dc = nx.average_degree_connectivity(G)
    connectivity = numpy.mean(list(dc.values()))
    diameter = nx.diameter(G)
    average_clustering = nx.average_clustering(G)

if __name__ == '__main__':
    cli()
