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
        max_expected_block_difference = max(((latest_update_time - ts) // datetime.timedelta(seconds=25)) + 10, 2)
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

def map_ip_to_data(geo_data):
    ip_geo_map = {}
    for node in geo_data:
        ip_geo_map[node['query']] = node
    return ip_geo_map

@cli.command()
@click.option('--endpoint', help='network data endpoint')
def network_map(endpoint):
    r = requests.get(endpoint)


def create_map(data, dt):
    valid_addresses = []
    valid_nodes = []
    for node in data:
        valid_addresses.append(node['address'].split(':')[0])
        valid_nodes.append(node)
    geo_data = geo_locate_ips(valid_addresses)
    ip_data_map = map_ip_to_data(geo_data)
    df = pandas.DataFrame(geo_data)

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
                    yield pandas.DataFrame([
                        {
                        'start_lat': ip_data_map[start]['lat'],
                        'start_lon': ip_data_map[start]['lon'],
                        'end_lat': ip_data_map[end]['lat'],
                        'end_lon': ip_data_map[end]['lon']
                         },
                    ])

    df_links = pandas.concat(one())

    dfs = pandas.read_csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')


    df_nodes = df.groupby(['lat', 'lon']).first()
    cnt = df.groupby(['lat', 'lon']).count()
    df_nodes = df_nodes.join(cnt[['status']].rename(columns={'status':'count'}))
    df_nodes['node_size'] = (((df_nodes['count'] // df_nodes['count'].max()) * 20) + 5) * 1.5
    df_nodes = df_nodes.reset_index()

    country_count = df.groupby('country').count()[['query']]
    df_cnt = df.merge(country_count.rename(columns={'query':'node_count'}), left_on='country', right_index=True, how='left')
    df_countries = df_cnt.groupby('country').first().reset_index()

    # fig = go.Figure()

    fig = go.Figure(data=go.Choropleth(
        locations=df_countries['country'],
        locationmode='country names',
        z=df_countries['node_count'],
        zmax=200,
        text=df_countries['country'],
        colorscale= px.colors.sequential.Oranges[2:],
        autocolorscale=False,
        reversescale=False,
        marker_line_color='darkgray',
        marker_line_width=0.5,
        colorbar_title='Nodes',
    ))

    for i, row in df_links.iterrows():
        fig.add_trace(
            go.Scattergeo(
                # locationmode='USA-states',
                lon=[row['start_lon'], row['end_lon']],
                lat=[row['start_lat'], row['end_lat']],
                mode='lines',
                line=dict(width=0.5, color='rgb(240, 240, 240)'),
                opacity=0.3,
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
            color='rgb(0, 0, 0)',
            line=dict(
                width=3,
                color='rgba(68, 68, 68, 0)'
            )
        )))


    fig.add_annotation(x=0.075, y=0.2,
                       text=f"Countries: {len(df_countries)}",
                       showarrow=False,
                       font=dict(
                           family="Courier New, monospace",
                           size=20,
                           color="white"
                       ),
                       opacity=1
                       )

    fig.add_annotation(x=0.075, y=0.25,
                       text=f"Node Count: {len(valid_nodes)}",
                       showarrow=False,
                       font=dict(
                           family="Courier New, monospace",
                           size=20,
                           color="white"
                       ),
                       opacity=1
                       )

    fig.add_annotation(x=0.075, y=0.3,
                       text=f"Date: {dt.strftime('%d %b %Y, %H:%M:%S')}",
                       showarrow=False,
                       font=dict(
                           family="Courier New, monospace",
                           size=20,
                           color="white"
                       ),
                       opacity=1
                       )
    fig.add_annotation(x=0.95, y=-0.03,
                       text=f"^ Node locations are approximate",
                       showarrow=False,
                       font=dict(
                            family="Courier New, monospace",
                            size=16,
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
        title={
            'text':f'Minima Global Node Map',
            'font_size':30,
            'x': 0.5,
            # 'xanchor': "center"
        },
        showlegend=False,
        geo=dict(
            # scope='north america',
            # projection_type='azimuthal equal area',
            showland=True,
            landcolor='rgb(145, 145, 145)',
            countrycolor='rgb(204, 204, 204)',
            showocean=True,
            # oceancolor='rgb(49, 122, 255)',
            oceancolor='rgb(0, 28, 50)',
            showcountries=True
        ),
    )

    fig.write_image(f"{dt.strftime('%Y%m%dT%H%M%S')}.png", width=2000, height=1000)


def geo_locate_ips(ips):
    batches = math.ceil(len(ips) / 100)
    geo_data = []
    for i in range(1, batches + 1):
        r = requests.post(
            "http://ip-api.com/batch?fields=status,message,country,countryCode,region,regionName,city,lat,lon,query",
            data=str(ips[(i - 1) * 100:(i * 100)]).replace('"', '').replace("'", '"'))
        geo_data += r.json()
    return geo_data

@cli.command()
@click.option('--endpoint', help='network data endpoint')
def maps(endpoint):
    # r = requests.get(endpoint)
    # valid_addresses = set()
    # valid_nodes = []
    # for node in r.json():
    #     major, minor, build = node['minima_version'].split('.')
    #     if (major == '0') and (minor == '100') and (int(build) >= 13):
    #         valid_addresses.add(node['address'].split(':')[0])

    # for node in sorted(valid_addresses):
    #     print(node)

    # geo = requests.get('https://app.ipapi.co/bulk/q=104.155.19.103%0D%0A109.37.159.90%0D%0A116.202.103.231%0D%0A135.181.152.131%0D%0A135.181.92.141%0D%0A139.59.137.30%0D%0A147.182.254.42%0D%0A147.78.66.147%0D%0A152.37.87.57%0D%0A161.97.127.66%0D%0A161.97.84.225%0D%0A164.68.96.105%0D%0A168.119.164.176%0D%0A176.249.17.125%0D%0A176.65.61.156%0D%0A178.150.235.149%0D%0A178.20.47.139%0D%0A185.194.219.116%0D%0A185.194.219.139%0D%0A185.195.27.137%0D%0A&key=&output=json')





    files = sorted(glob.glob('*.json'))[-4:]
    for file in files:
        print(file)
        dt = pandas.to_datetime('-'.join(file.split('.')[0].split('-')[2:])).to_pydatetime()
        with open(file) as json_file:
            data = json.load(json_file)
            create_map(data, dt)
            time.sleep(2)

    # import IPython
    # IPython.embed()

@cli.command()
def video():
    import imageio as iio
    from pygifsicle import optimize

    images = list()
    files = sorted(glob.glob('*.png'))
    for file in files:
        im = iio.imread(file)
        images.append(im)

    gif_path = "nodemap.gif"
    with iio.get_writer(gif_path, mode='I', fps=1) as writer:
        for i in images:
            writer.append_data(i)



    optimize(gif_path, "optimized_white.gif")



if __name__ == '__main__':
    cli()

