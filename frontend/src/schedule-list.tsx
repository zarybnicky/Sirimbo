import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import Form from 'react-bootstrap/Form';
import { ApolloProvider, useQuery, useMutation } from '@apollo/client';
import { Pagination } from './pagination';
import { DateEl } from './date';
import { gql } from 'graphql-tag';
import { createClient, UserQuery } from './client';
import { Dropdown } from './dropdown';

export const RozpisAdminQuery = gql(`
query ScheduleAdminList($offset: Int, $limit: Int) {
  allRozpis(first: $limit, offset: $offset, orderBy: R_DATUM_DESC) {
    nodes {
      rDatum
      rId
      rKde
      rLock
      rTimestamp
      rTrener
      rVisible
      userByRTrener {
        uId
        uJmeno
        uPrijmeni
      }
      rozpisItemsByRiIdRodic {
        nodes {
          riDo
          riOd
          riId
          riPartner
        }
      }
    }
    totalCount
  }
}`);

const ToggleVisibleRozpis = gql(`
mutation setRozpisVisible($id: BigInt!, $visible: Boolean!) {
  updateRozpiByRId(input: {rozpiPatch: {rVisible: $visible}, rId: $id}) {
    rozpi {
      rId
    }
  }
}`);

export function RozpisAdminList() {
  const [limit, setLimit] = useState(30);
  const [offset, setOffset] = useState(0);
  const [total, setTotal] = useState(0);
  const { data: user } = useQuery(UserQuery);
  const { data, refetch } = useQuery(RozpisAdminQuery, {
    variables: { limit, offset },
    onCompleted: (data) => {
      const total = data.allRozpis?.totalCount;
      total && setTotal(total);
    },
  });
  const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);
  const [toggleVisible] = useMutation(ToggleVisibleRozpis, {
    onCompleted: () => refetch(),
  });

  const list = (!user || !data?.allRozpis?.nodes.length) ? null : <table>
    <thead>
      <tr>
        <th>Trenér</th>
        <th>Datum</th>
        <th>Místo</th>
        <th>Viditelný</th>
      </tr>
    </thead>
    <tbody>
      {data!.allRozpis.nodes.filter(
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

export class RozpisAdminListElement extends HTMLElement {
  connectedCallback() {
    ReactDOM.render(
      <ApolloProvider client={createClient()}><RozpisAdminList /></ApolloProvider>,
      this
    );
  }
}
