import * as React from 'react';
import * as ReactDOM from 'react-dom';

interface Nabidka {
    n_od: string;
    n_do: string;
    u_jmeno: string;
    u_prijmeni: string;
    n_pocet_hod: number;
    n_max_pocet_hod: number;
    hourReserved: number;
    canEdit: boolean;
    items: Array<{
        ni_pocet_hod: string;
        u_jmeno: string;
        u_prijmeni: string;
    }>;
}

export class ReservationSelect extends React.Component<{}, {
    id?: string,
    data?: Nabidka,
    options: Array<{ key: string, value: string }>,
}> {
    constructor(props: {}) {
        super(props)
        this.state = {
            options: JSON.parse(document.querySelector('#reservation-options')!.textContent!),
        };
    }

    handleOnChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
        if (event.target.value == 'none') {
            this.setState({});
        }
        this.setState({ id: event.target.value });
        fetch(`/api/reservation/${event.target.value}`)
            .then(x => x.json())
            .then(x => this.setState({ data: x }));
    }

    override render = () => (
        <div>
            <select className='team-selection' value={this.state.id || 'none'} onChange={this.handleOnChange} >
                <option value='none'> --vyberte nabídku-- </option>
                {this.state.options.map(x =>
                    <option value={x.key} key={x.value}> {x.value} </option>
                )}
            </select>
            {this.state.data && <div>
                <div className="trenink-header">
                    <div className="title">
                        {this.state.data.u_jmeno} {this.state.data.u_prijmeni}
                        {this.state.data.canEdit && <div className="btn-group">
                            <button type="button" className="btn btn-xs pt-0" data-toggle="dropdown" >
                                <img alt="Upravit" width="16" src="/style/icon-gear.png" />
                            </button>
                            <div className="dropdown-menu dropdown-menu-right" >
                                <a className="dropdown-item" href="/admin/nabidka/edit/{{ this.state.id }}" > Upravit </a>,
                                <a className="dropdown-item" href="/admin/nabidka/detail/{{ this.state.id }}" > Upravit rezervace </a>
                            </div>
                        </div>}
                    </div>
                    <div className="date" >
                        {this.state.data.n_od}
                        {this.state.data.n_do && this.state.data.n_od != this.state.data.n_do && this.state.data.n_do}
                    </div>
                    {
                        this.state.data.n_max_pocet_hod > 0 && <div>
                            <span className="little"> Maximálně hodin/ pár: </span>
                            <span className="nadpis" > {this.state.data.n_max_pocet_hod} </span>
                        </div>}
                    <div>
                        <span className="little" > Volných hodin: </span>
                        <span className="nadpis">
                            {this.state.data.n_pocet_hod - this.state.data.hourReserved} z
                            {this.state.data.n_pocet_hod} nabízených
                        </span>
                    </div>
                </div>
                <table style={{ margin: '1rem auto 0' }}>
                    <tr><th>Tanečník </th><th>Počet hodin</th></tr>
                    {
                        this.state.data.items.map(item => <tr>
                            <td>{item.u_jmeno} {item.u_prijmeni}</td>
                            <td> {item.ni_pocet_hod} </td>
                        </tr>)}
                </table>
            </div>}
        </div>
    )
}

if (document.getElementById('reservation-select')) {
    ReactDOM.render(<ReservationSelect />, document.getElementById('reservation-select'));
}
