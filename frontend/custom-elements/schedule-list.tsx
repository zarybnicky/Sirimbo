import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { ApolloProvider, HttpLink, ApolloClient, InMemoryCache } from '@apollo/client';
import Form from 'react-bootstrap/Form';
import { Pagination } from './pagination';
import { DateEl } from './date';
import { UserQuery } from 'lib/data/use-auth';
import { Dropdown } from './dropdown';
import { $, RozpisOrderBy, Selector } from 'lib/zeus';
import { useTypedQuery, useTypedMutation } from 'lib/zeus/apollo';

export const ScheduleListQuery = Selector('Query')({
  rozpis: [
    { first: $`limit`, offset: $`offset`, orderBy: [RozpisOrderBy.R_DATUM_DESC] },
    {
      nodes: {
        rDatum: true,
        rId: true,
        rKde: true,
        rLock: true,
        rTimestamp: true,
        rTrener: true,
        rVisible: true,
        userByRTrener: {
          uId: true,
          uJmeno: true,
          uPrijmeni: true,
        },
        rozpisItemsByRiIdRodic: [{}, {
          nodes: {
            riDo: true,
            riOd: true,
            riId: true,
            riPartner: true,
          },
        }],
      },
      totalCount: true,
    }
  ],
});

export const ToggleScheduleVisible = Selector('Mutation')({
  updateRozpi: [
    { input: { patch: { rVisible: $`visible` }, rId: $`id` } },
    {
      rozpi: {
        rId: true,
      }
    }
  ],
});

export function RozpisAdminList() {
  const [limit] = React.useState(30);
  const [offset, setOffset] = React.useState(0);
  const [total, setTotal] = React.useState(0);
  const { data: user } = useTypedQuery(UserQuery);
  const { data, refetch } = useTypedQuery(ScheduleListQuery, {
    variables: { limit, offset },
    onCompleted: (data) => {
      const total = data.rozpis?.totalCount;
      total && setTotal(total);
    },
  });
  const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);
  const [toggleVisible] = useTypedMutation(ToggleScheduleVisible, {
    onCompleted: () => refetch(),
  });

  const list = (!user || !data?.rozpis?.nodes.length) ? null : <table>
    <thead>
      <tr>
        <th>Trenér</th>
        <th>Datum</th>
        <th>Místo</th>
        <th>Viditelný</th>
      </tr>
    </thead>
    <tbody>
      {data!.rozpis.nodes.filter(
        a => 16 <= (user.getCurrentUser?.permissionByUGroup?.peRozpis || 0)
          || a.rTrener == user.getCurrentUser?.uId
      ).map((a) => <tr key={a.rId}>
        <td>
          <Dropdown links={{
            [`/admin/rozpis/edit/${a.rId}`]: "Upravit",
            [`/admin/rozpis/detail/${a.rId}`]: "Upravit lekce",
            [`/admin/rozpis/duplicate/${a.rId}`]: "Duplikovat",
            [`/admin/rozpis/remove/${a.rId}`]: "Odstranit",
          }} />
          {a.userByRTrener?.uJmeno} {a.userByRTrener?.uPrijmeni}
        </td>
        <td><DateEl date={a.rDatum} /></td>
        <td>{a.rKde}</td>
        <td>
          <Form.Check checked={a.rVisible || false} onChange={() => toggleVisible({
            variables: { id: a.rId, visible: !a.rVisible },
          })} />
        </td>
      </tr>)}
    </tbody>
  </table >;

  return <React.Fragment>
    <a href="/admin/rozpis/add" className="btn btn-primary">Nový rozpis</a>
    {list}
    <Pagination {...{ total, limit, setPage }} />
  </React.Fragment>;
}

const client = new ApolloClient({
  link: new HttpLink({ uri: '/graphql' }),
  cache: new InMemoryCache(),
});

export class RozpisAdminListElement extends HTMLElement {
  connectedCallback() {
    ReactDOM.render(
      <ApolloProvider client={client}><RozpisAdminList /></ApolloProvider>,
      this
    );
  }
}
