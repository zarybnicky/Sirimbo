import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useEffect, useState } from 'react';
import { DefaultApi, ReservationResponse } from './schema';
import { format } from 'date-fns';

const ReservationView = ({ trainer, reservation, items }: ReservationResponse) => {
    const header = <div className="trenink-header">
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
            <span className="little"> Maximálně hodin/pár: </span>
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
    </div>;
    const content = <table className="nocolor" style={{ width: '100%' }}>
        <thead>
            <tr><th>Tanečník</th><th>Počet hodin</th></tr>
        </thead>
        <tbody>
            {items.map(item => <tr><td>{item[0]}</td><td>{item[1]}</td></tr>)}
        </tbody>
    </table>;

    return <div className="col-12 col-md-6 col-lg-4 pb-2">
        <div className="widget">
            <div className="widget-title text-center">{header}</div>
            <div className="widget-content">{content}</div>
        </div>
    </div>;
}

export function ReservationSelect() {
    const [id, setId] = useState<string | null>(null);
    const [options, setOptions] = useState<{ key: string; value: string; }[]>([]);
    const [current, setCurrent] = useState<ReservationResponse | null>(null);

    useEffect(() => {
        setOptions(JSON.parse(document.querySelector('#reservation-options')!.textContent!))
    }, []);

    const onChange = async (event: React.ChangeEvent<HTMLSelectElement>) => {
        if (event.target.value == 'none') {
            setId(null);
            setCurrent(null);
            return;
        }
        setId(event.target.value);
        setCurrent(await new DefaultApi().getReservation({ id: parseInt(event.target.value, 10) }));
    }

    return <div>
        <select className='team-selection' value={id || 'none'} onChange={onChange}>
            <option value='none'> --vyberte nabídku-- </option>
            {options.map(x => <option value={x.key} key={x.key}>{x.value}</option>)}
        </select>
        {current ? ReservationView(current) : null}
    </div>;
}


class ReservationSelectElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(<ReservationSelect />, this);
    }
}
customElements.define('reservation-select', ReservationSelectElement);
