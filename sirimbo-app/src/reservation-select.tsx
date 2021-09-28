import * as React from 'react';
import * as ReactDOM from 'react-dom';

interface ReservationResponse {
    trainer: User;
    reservation: Nabidka;
    items: [NabidkaItem];
}
interface NabidkaItem {
    lock: "0" | "1";
    numberLessons: number;
    partner: number;
}
interface User {
    ban: "0" | "1";
    birthDate: string;
    city: string;
    confirmed: "0" | "1";
    conscriptionNumber: string;
    createdAt: string;
    dancer: "0" | "1";
    district: string;
    email: string;
    gdprSignedAt: string;
    gender: "m" | "f";
    level: number;
    lock: "0" | "1";
    login: string;
    memberSince: string;
    memberUntil: string;
    name: string;
    nationality: string;
    notes: string;
    orientationNumber: string;
    paymentGroup: number;
    phone: string;
    postalCode: string;
    street: string;
    surname: string;
    system: "0" | "1";
    teacher: "0" | "1";
    updatedAt: string;
    userGroup: number;
}
interface Nabidka {
    from: string;
    lock: "0" | "1";
    maximumPerPair: number;
    numberLessons: number;
    to: string;
    updatedAt: string;
    visible: "0" | "1";
}

export class ReservationSelect extends React.Component<{}, {
    id?: string,
    data?: ReservationResponse,
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
            .then((data: ReservationResponse) => this.setState({ data }));
    }

    override render = () => (
        <div>
            <select className='team-selection' value={this.state.id || 'none'} onChange={this.handleOnChange} >
                <option value='none'> --vyberte nabídku-- </option>
                {this.state.options.map(x =>
                    <option value={x.key} key={x.key}> {x.value} </option>
                )}
            </select>
            {this.state.data && <div>
                <div className="trenink-header">
                    <div className="title">
                        {this.state.data.trainer.name} {this.state.data.trainer.surname}
                        {/* {this.state.data.canEdit && <div className="btn-group">
                          <button type="button" className="btn btn-xs pt-0" data-toggle="dropdown" >
                          <img alt="Upravit" width="16" src="/style/icon-gear.png" />
                          </button>
                          <div className="dropdown-menu dropdown-menu-right" >
                          <a className="dropdown-item" href="/admin/nabidka/edit/{{ this.state.id }}" > Upravit </a>,
                          <a className="dropdown-item" href="/admin/nabidka/detail/{{ this.state.id }}" > Upravit rezervace </a>
                          </div>
                          </div>} */}
                    </div>
                    <div className="date" >
                        {this.state.data.n_od}
                        {this.state.data.n_do && this.state.data.n_od != this.state.data.n_do && this.state.data.n_do}
                    </div>
                    {this.state.data.reservation.maximumPerPair > 0 && <div>
                        <span className="little"> Maximálně hodin/ pár: </span>
                        <span className="nadpis" > {this.state.data.reservation.maximumPerPair} </span>
                    </div>}
                    <div>
                        <span className="little" > Volných hodin: </span>
                        <span className="nadpis">
                            {this.state.data.n_pocet_hod - this.state.data.reservation} z
                            {this.state.data.n_pocet_hod} nabízených
                        </span>
                    </div>
                </div>
                <table style={{ margin: '1rem auto 0' }}>
                    <tr><th>Tanečník </th><th>Počet hodin</th></tr>
                    {this.state.data.items.map(item => <tr>
                        <td>{item.u_jmeno} {item.u_prijmeni}</td>
                        <td> {item.numberLessons} </td>
                    </tr>)}
                </table>
            </div>}
        </div>
    )
}

if (document.getElementById('reservation-select')) {
    ReactDOM.render(<ReservationSelect />, document.getElementById('reservation-select'));
}
