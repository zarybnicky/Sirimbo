import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import Form from 'react-bootstrap/Form';
import { ApolloProvider, useQuery, useMutation } from '@apollo/client';
import { Pagination } from './pagination';
import { DateRange } from './date';
import { gql } from 'graphql-tag';
import { createClient, UserQuery } from './client';
import { Dropdown } from './dropdown';

export const NabidkaAdminQuery = gql(`
query ReservationAdminList($offset: Int, $limit: Int) {
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

const ToggleVisibleNabidka = gql(`
mutation setNabidkaVisible($id: BigInt!, $visible: Boolean!) {
  updateNabidkaByNId(input: {nabidkaPatch: {nVisible: $visible}, nId: $id}) {
    nabidka {
      nId
    }
  }
}`);

export function ReservationAdminList() {
  const [limit, setLimit] = useState(30);
  const [offset, setOffset] = useState(0);
  const [total, setTotal] = useState(0);
  const { data: user } = useQuery(UserQuery);
  const { data, refetch } = useQuery(NabidkaAdminQuery, {
    variables: { limit, offset },
    onCompleted: (data) => {
      const total = data.allNabidkas?.totalCount;
      total && setTotal(total);
    },
  });
  const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);
  const [toggleVisible] = useMutation(ToggleVisibleNabidka, {
    onCompleted: () => refetch(),
  });

  const list = (!user || !data?.allNabidkas?.nodes.length) ? null : <table>
    <thead>
      <tr>
        <th>Trenér</th>
        <th>Datum</th>
        <th>Viditelný</th>
      </tr>
    </thead>
    <tbody>
      {data!.allNabidkas?.nodes.filter(
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
          <Form.Check checked={a.nVisible || false} onChange={() => toggleVisible({
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

export class ReservationAdminListElement extends HTMLElement {
  connectedCallback() {
    ReactDOM.render(
      <ApolloProvider client={createClient()}><ReservationAdminList /></ApolloProvider>,
      this
    );
  }
}
