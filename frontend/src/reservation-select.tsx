import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import { ApolloProvider, useQuery } from '@apollo/client';
import { formatDateRange } from './date';
import { createClient } from './client';
import { gql } from 'graphql-tag';
import { Nabidka } from './graphql/graphql';

const ReservationView = (x: Nabidka) => {
  const header = <div className="trenink-header">
    <div className="title">
      {x?.userByNTrener?.uJmeno} {x?.userByNTrener?.uPrijmeni}
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
    <div className="date">{formatDateRange(x?.nOd, x?.nDo, '1')}</div>
    {x?.nMaxPocetHod > 0 && <div>
      <span className="little"> Maximálně hodin/pár: </span>
      <span className="nadpis">{x?.nMaxPocetHod}</span>
    </div>}
    <div>
      <span className="little">Volných hodin: </span>
      <span className="nadpis">
        {x?.nPocetHod - (x.nabidkaItemsByNiIdRodic.nodes || []).reduce((x, y) => x + y.niPocetHod, 0)}
        {" z "}
        {x?.nPocetHod} nabízených
      </span>
    </div>
  </div>;
  const content = <table className="nocolor" style={{ width: '100%' }}>
    <thead>
      <tr><th>Tanečník</th><th>Počet hodin</th></tr>
    </thead>
    <tbody>
      {(x.nabidkaItemsByNiIdRodic.nodes || []).map(item => <tr>
        <td>{item.paryByNiPartner?.userByPIdPartner?.uJmeno} {item.paryByNiPartner?.userByPIdPartner?.uPrijmeni}</td>
        <td>{item.niPocetHod}</td>
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
  allNabidkas(first: $limit, offset: $offset, orderBy: N_OD_DESC) {
    nodes {
      nDo
      nId
      nLock
      nMaxPocetHod
      nOd
      nPocetHod
      nTimestamp
      nTrener
      nVisible
      nabidkaItemsByNiIdRodic {
        nodes {
          niPocetHod
          niPartner
          niLock
          paryByNiPartner {
            userByPIdPartner {
              uJmeno
              uPrijmeni
              uId
            }
          }
        }
      }
      userByNTrener {
        uJmeno
        uPrijmeni
        uId
      }
    }
    totalCount
  }
}`);

export function ReservationSelect() {
  const { data: reservations } = useQuery(NabidkaList);
  const [reservation, setReservation] = useState<Nabidka | undefined>();
  const onChange = async (event: React.ChangeEvent<HTMLSelectElement>) => {
    const id = parseInt(event.target.value, 10);
    setReservation(reservations?.allNabidkas?.nodes?.find(x => x.nId === id) as Nabidka);
  };

  return <div>
    <select className='team-selection' value={reservation?.nId || 'none'} onChange={onChange}>
      <option value='none'> --vyberte nabídku-- </option>
      {(reservations?.allNabidkas?.nodes || []).map(x => <option value={x.nId} key={x.nId}>
        {`${formatDateRange(x.nOd, x.nDo)} - ${x.userByNTrener?.uJmeno} ${x.userByNTrener?.uPrijmeni}`}
      </option>)}
    </select>
    {reservation ? ReservationView(reservation) : null}
  </div>;
}

export class ReservationSelectElement extends HTMLElement {
  connectedCallback() {
    ReactDOM.render(
      <ApolloProvider client={createClient()}><ReservationSelect /></ApolloProvider>,
      this
    );
  }
}