import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { ApolloProvider, HttpLink, ApolloClient, InMemoryCache } from '@apollo/client';
import { $, Selector, NabidkasOrderBy, ModelTypes } from 'lib/zeus';
import { useTypedQuery } from 'lib/zeus/apollo';
import { formatDateRange } from './date';
import { scalars } from 'lib/apollo';

const ReservationView = (x: ModelTypes["Nabidka"]) => {
  const header = <div className="trenink-header">
    <div className="title">
      {x.userByNTrener?.uJmeno} {x.userByNTrener?.uPrijmeni}
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

export const NabidkaList = Selector("Query")({
  nabidkas: [
    {
      first: $('limit', 'Int!'),
      offset: $('offset', 'Int!'),
      orderBy: [NabidkasOrderBy.N_OD_DESC],
    },
    {
      nodes: {
        nDo: true,
        nId: true,
        nLock: true,
        nMaxPocetHod: true,
        nOd: true,
        nPocetHod: true,
        nTimestamp: true,
        nTrener: true,
        nVisible: true,
        nabidkaItemsByNiIdRodic: [{}, {
          nodes: {
            niPocetHod: true,
            niPartner: true,
            niLock: true,
            paryByNiPartner: {
              userByPIdPartner: {
                uJmeno: true,
                uPrijmeni: true,
                uId: true,
              },
            },
          },
        }],
        userByNTrener: {
          uJmeno: true,
          uPrijmeni: true,
          uId: true,
        },
      },
      totalCount: true,
    },
  ],
});

export function ReservationSelect() {
  const { data: reservations } = useTypedQuery(NabidkaList, { scalars });
  const [reservation, setReservation] = React.useState<ModelTypes["Nabidka"] | undefined>();
  const onChange = async (event: React.ChangeEvent<HTMLSelectElement>) => {
    const id = parseInt(event.target.value, 10);
    setReservation(reservations?.nabidkas?.nodes?.find(x => x.nId === id) as ModelTypes["Nabidka"]);
  };

  return <div>
    <select className='team-selection' value={reservation?.nId || 'none'} onChange={onChange}>
      <option value='none'> --vyberte nabídku-- </option>
      {reservations?.nabidkas?.nodes?.map(x => (
        <option value={x.nId} key={x.nId}>
          {formatDateRange(x.nOd, x.nDo)} - {x.userByNTrener?.uJmeno} {x.userByNTrener?.uPrijmeni}
        </option>
      ))}
    </select>
    {reservation ? ReservationView(reservation) : null}
  </div>;
}

const client = new ApolloClient({
  link: new HttpLink({ uri: '/graphql' }),
  cache: new InMemoryCache(),
});

export class ReservationSelectElement extends HTMLElement {
  connectedCallback() {
    ReactDOM.render(
      <ApolloProvider client={client}><ReservationSelect /></ApolloProvider>,
      this
    );
  }
}
