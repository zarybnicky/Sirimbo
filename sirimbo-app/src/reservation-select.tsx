import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { ApolloProvider, useQuery, useLazyQuery } from '@apollo/client';
import { formatDateRange } from './date';
import { createClient } from './client';
import { gql } from 'graphql-tag';
import { Nabidka } from './schedule-visible';
import { NabidkaQuery } from './graphql/graphql';

const ReservationView = (x: NabidkaQuery['nabidka_by_pk']) => {
    const header = <div className="trenink-header">
        <div className="title">
            {x?.user?.u_jmeno} {x?.user?.u_prijmeni}
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
        <div className="date">{formatDateRange(x?.n_od, x?.n_do, '1')}</div>
        {x?.n_max_pocet_hod > 0 && <div>
            <span className="little"> Maximálně hodin/pár: </span>
            <span className="nadpis">{x?.n_max_pocet_hod}</span>
        </div>}
        <div>
            <span className="little">Volných hodin: </span>
            <span className="nadpis">
                {x?.n_pocet_hod - (x?.nabidka_items || []).reduce((x, y) => x + y.ni_pocet_hod, 0)}
                {" z "}
                {x?.n_pocet_hod} nabízených
            </span>
        </div>
    </div>;
    const content = <table className="nocolor" style={{ width: '100%' }}>
        <thead>
            <tr><th>Tanečník</th><th>Počet hodin</th></tr>
        </thead>
        <tbody>
            {(x?.nabidka_items || []).map(item => <tr>
                <td>{item.pary.user.u_jmeno} {item.pary.user.u_prijmeni}</td>
                <td>{item.ni_pocet_hod}</td>
            </tr>)}
        </tbody>
    </table>;

    return <div className="col-12 col-md-6 col-lg-4 pb-2">
        <div className="widget">
            <div className="widget-title text-center">{header}</div>
            <div className="widget-content">{content}</div>
        </div>
    </div>;
}

export const NabidkaList = gql(`
query NabidkaList($offset: Int, $limit: Int) {
  nabidka(limit: $limit, offset: $offset) {
    n_visible
    n_trener
    n_timestamp
    n_pocet_hod
    n_od
    n_max_pocet_hod
    n_lock
    n_id
    n_do
    user {
      u_jmeno
      u_prijmeni
      u_id
    }
    nabidka_items {
      ni_lock
      ni_partner
      ni_pocet_hod
      pary {
        user {
          u_id
          u_jmeno
          u_prijmeni
        }
      }
    }
  }
}`);

export function ReservationSelect() {
    const { data: reservations } = useQuery(NabidkaList);
    const [loadReservation, { loading, error, data }] = useLazyQuery(Nabidka);
    const onChange = async (event: React.ChangeEvent<HTMLSelectElement>) => {
        const id = parseInt(event.target.value, 10);
        if (id) {
            loadReservation({ variables: { id } });
        }
    };

    return <div>
        <select className='team-selection' value={data?.nabidka_by_pk?.n_id || 'none'} onChange={onChange}>
            <option value='none'> --vyberte nabídku-- </option>
            {(reservations?.nabidka || []).map(x => <option value={x.n_id} key={x.n_id}>
                {`${formatDateRange(x.n_od, x.n_do)} - ${x.user.u_jmeno} ${x.user.u_prijmeni}`}
            </option>)}
        </select>
        {data?.nabidka_by_pk ? ReservationView(data.nabidka_by_pk) : null}
    </div>;
}

class ReservationSelectElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(
            <ApolloProvider client={createClient()}><ReservationSelect /></ApolloProvider>,
            this
        );
    }
}
customElements.define('reservation-select', ReservationSelectElement);
