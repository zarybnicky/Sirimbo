import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { components } from './schema';
import { format } from 'date-fns';

type ReservationResponse = components['schemas']['ReservationResponse'];

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
            .then((data: ReservationResponse) => {
                console.log(data);
                console.log(data.reservation);
                console.log(data.reservation.reservationFrom);
                console.log(new Date(data.reservation.reservationFrom));
                this.setState({ data });
            });
    }

    viewReservation = ({ trainer, reservation, items }: ReservationResponse) =>
        <div className="col-12 col-md-6 col-lg-4 pb-2">
            <div className="widget">
                <div className="widget-title text-center">
                    <div className="trenink-header">
                        <div className="title">
                            {trainer.userName} {trainer.userSurname}
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
                            {format(new Date(reservation.reservationFrom), "d.M.")}
                            {reservation.reservationTo &&
                                reservation.reservationTo != reservation.reservationFrom &&
                                ` - ${format(new Date(reservation.reservationTo), "d.M.")}`}
                        </div>
                        {reservation.reservationMaximumPerPair > 0 && <div>
                            <span className="little"> Maximálně hodin/ pár:</span>
                            <span className="nadpis">{reservation.reservationMaximumPerPair}</span>
                        </div>}
                        <div>
                            <span className="little">Volných hodin:</span>
                            <span className="nadpis">
                                {reservation.reservationNumberLessons - items.reduce((x, y) => x + y[1], 0)}
                                {" z "}
                                {reservation.reservationNumberLessons} nabízených
                            </span>
                        </div>
                    </div>
                </div>
                <div className="widget-content">
                    <table className="nocolor" style={{ width: '100%' }}>
                        <thead>
                            <tr><th>Tanečník</th><th>Počet hodin</th></tr>
                        </thead>
                        <tbody>
                            {items.map(item => <tr><td>{item[0]}</td><td>{item[1]}</td></tr>)}
                        </tbody>
                    </table>
                </div>
            </div>
        </div>

    override render = () => <div>
        <select className='team-selection' value={this.state.id || 'none'} onChange={this.handleOnChange}>
            <option value='none'> --vyberte nabídku-- </option>
            {this.state.options.map(x => <option value={x.key} key={x.key}>{x.value}</option>)}
        </select>
        {this.state.data ? this.viewReservation(this.state.data) : null}
    </div>
}

if (document.getElementById('reservation-select')) {
    ReactDOM.render(<ReservationSelect />, document.getElementById('reservation-select'));
}
