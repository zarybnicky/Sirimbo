import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import { ApolloProvider, HttpLink, ApolloClient, InMemoryCache } from '@apollo/client';
import { Pagination } from './pagination';
import { DateRange } from './date';
import { UserQuery } from '../use-auth';
import { Dropdown } from './dropdown';
import { $, NabidkasOrderBy, Selector } from '../zeus';
import { useTypedQuery, useTypedMutation } from '../zeus/apollo';

export const NabidkaAdminQuery = Selector('Query')({
  nabidkas: [
    { first: $`limit`, offset: $`offset`, orderBy: [NabidkasOrderBy.N_OD_DESC] },
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

const ToggleVisibleNabidka = Selector("Mutation")({
  updateNabidka: [
    { input: { patch: { nVisible: $`visible` }, nId: $`id` } },
    {
      nabidka: {
        nId: true,
      },
    },
  ],
});

export function ReservationAdminList() {
  const [limit] = useState(30);
  const [offset, setOffset] = useState(0);
  const [total, setTotal] = useState(0);
  const { data: user } = useTypedQuery(UserQuery);
  const { data, refetch } = useTypedQuery(NabidkaAdminQuery, {
    variables: { limit, offset },
    onCompleted: (data) => {
      const total = data.nabidkas?.totalCount;
      total && setTotal(total);
    },
  });
  const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);
  const [toggleVisible] = useTypedMutation(ToggleVisibleNabidka, {
    onCompleted: () => refetch(),
  });

  const list = (!user || !data?.nabidkas?.nodes.length) ? null : <table>
    <thead>
      <tr>
        <th>Trenér</th>
        <th>Datum</th>
        <th>Viditelný</th>
      </tr>
    </thead>
    <tbody>
      {data!.nabidkas?.nodes.filter(
        a => 16 <= (user.getCurrentUser?.permissionByUGroup?.peNabidka || 0)
          || a.nTrener == user.getCurrentUser?.uId
      ).map((a) => <tr key={a.nId}>
        <td>
          <Dropdown links={{
            [`/admin/nabidka/edit/${a.nId}`]: "Upravit",
            [`/admin/nabidka/detail/${a.nId}`]: "Upravit lekce",
            [`/admin/nabidka/duplicate/${a.nId}`]: "Duplikovat",
            [`/admin/nabidka/remove/${a.nId}`]: "Odstranit",
          }} />
          {a.userByNTrener?.uJmeno} {a.userByNTrener?.uPrijmeni}
        </td>
        <td><DateRange from={a.nOd} to={a.nDo} /></td>
        <td>
          <input type="checkbox" checked={a.nVisible} onChange={() => toggleVisible({
            variables: { id: a.nId, visible: !a.nVisible },
          })} />
        </td>
      </tr>)}
    </tbody>
  </table >;

  return <React.Fragment>
    <a href="/admin/nabidka/add" className="btn btn-primary">Nová nabídka</a>
    {list}
    <Pagination {...{ total, limit, setPage }} />
  </React.Fragment>;
}

const client = new ApolloClient({
  link: new HttpLink({ uri: '/graphql' }),
  cache: new InMemoryCache(),
});

export class ReservationAdminListElement extends HTMLElement {
  connectedCallback() {
    ReactDOM.render(
      <ApolloProvider client={client}><ReservationAdminList /></ApolloProvider>,
      this
    );
  }
}
