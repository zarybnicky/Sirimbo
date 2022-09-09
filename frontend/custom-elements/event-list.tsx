import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import { ApolloProvider, HttpLink, ApolloClient, InMemoryCache } from '@apollo/client';
import Form from 'react-bootstrap/Form';
import { Pagination } from './pagination';
import { DateRange } from './date';
import { Dropdown } from './dropdown';
import { $, AkcesOrderBy, Selector } from '../zeus';
import { useTypedQuery, useTypedMutation } from '../zeus/apollo';

export const AkceList = Selector('Query')({
  akces: [
    { first: $`limit`, offset: $`offset`, orderBy: [AkcesOrderBy.A_OD_DESC] },
    {
      nodes: {
        aDo: true,
        aId: true,
        aInfo: true,
        aDokumenty: true,
        aJmeno: true,
        aKapacita: true,
        aKde: true,
        aLock: true,
        aOd: true,
        aTimestamp: true,
        aVisible: true,
        akceItemsByAiIdRodic: [{}, {
          nodes: {
            aiId: true,
            userByAiUser: {
              uJmeno: true,
              uPrijmeni: true,
              uId: true,
            },
          },
          totalCount: true,
        }],
      },
      totalCount: true,
    },
  ],
});

export const ToggleVisible = Selector('Mutation')({
  updateAkce: [
    { input: { aId: $`id`, patch: { aVisible: $`visible` } } },
    {
      akce: {
        aId: true,
      },
    },
  ],
});

export function EventList() {
  const [limit, setLimit] = useState(30);
  const [offset, setOffset] = useState(0);
  const [total, setTotal] = useState(0);
  const { data, refetch } = useTypedQuery(AkceList, {
    variables: { limit, offset },
    onCompleted: (data) => {
      const total = data.akces?.totalCount;
      total && setTotal(total);
    },
  });
  const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);
  const [toggleVisible] = useTypedMutation(ToggleVisible, {
    onCompleted: () => refetch(),
  });

  const list = !data?.akces?.nodes.length ? null : <table>
    <thead>
      <tr>
        <th>Jméno</th>
        <th>Datum</th>
        <th>Kapacita</th>
        <th>Viditelný</th>
      </tr>
    </thead>
    <tbody>
      {data!.akces.nodes.map((a) => <tr key={a.aId}>
        <td>
          <Dropdown links={{
            [`/admin/akce/edit/${a.aId}`]: "Upravit",
            [`/admin/akce/detail/${a.aId}`]: "Upravit účastníky",
            [`/admin/akce/dokumenty/${a.aId}`]: "Upravit dokumenty",
            [`/admin/akce/remove/${a.aId}`]: "Odstranit",
          }} />
          {a.aJmeno}
        </td>
        <td><DateRange from={a.aOd} to={a.aDo} /></td>
        <td>{a.akceItemsByAiIdRodic.totalCount || 0}/{a.aKapacita}</td>
        <td>
          <Form.Check checked={a.aVisible} onChange={() => toggleVisible({
            variables: { id: a.aId, visible: !a.aVisible },
          })} />
        </td>
      </tr>)}
    </tbody>
  </table>;

  return <React.Fragment>
    <a href="/admin/akce/add" className="btn btn-primary">Přidat</a>
    {list}
    <Pagination {...{ total, limit, setPage }} />
  </React.Fragment>;
}

const client = new ApolloClient({
  link: new HttpLink({ uri: '/graphql' }),
  cache: new InMemoryCache(),
});

export class EventListElement extends HTMLElement {
  connectedCallback() {
    ReactDOM.render(
      <ApolloProvider client={client}><EventList /></ApolloProvider>,
      this
    );
  }
}
