import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import Form from 'react-bootstrap/Form';
import { ApolloProvider, useQuery, useMutation } from '@apollo/client';
import { createClient } from '../client';
import { Pagination } from './pagination';
import { DateRange } from './date';
import { gql } from 'graphql-tag';
import { Dropdown } from './dropdown';

export const AkceList = gql(`
query AkceList($offset: Int, $limit: Int) {
  allAkces(first: $limit, offset: $offset, orderBy: A_OD_DESC) {
    nodes {
      aDo
      aId
      aInfo
      aDokumenty
      aJmeno
      aKapacita
      aKde
      aLock
      aOd
      aTimestamp
      aVisible
      akceItemsByAiIdRodic {
        nodes {
          aiId
          userByAiUser {
            uJmeno
            uPrijmeni
            uId
          }
        }
        totalCount
      }
    }
    totalCount
  }
}`);

const ToggleVisible = gql(`
  mutation setAkceVisible($id: BigInt!, $visible: Boolean!) {
    updateAkceByAId(input: {aId: $id, akcePatch: {aVisible: $visible}}) {
      akce {
        aId
      }
    }
  }
`);

export function EventList() {
  const [limit, setLimit] = useState(30);
  const [offset, setOffset] = useState(0);
  const [total, setTotal] = useState(0);
  const { data, refetch } = useQuery(AkceList, {
    variables: { limit, offset },
    onCompleted: (data) => {
      const total = data.allAkces?.totalCount;
      total && setTotal(total);
    },
  });
  const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);
  const [toggleVisible] = useMutation(ToggleVisible, {
    onCompleted: () => refetch(),
  });

  const list = !data?.allAkces?.nodes.length ? null : <table>
    <thead>
      <tr>
        <th>Jméno</th>
        <th>Datum</th>
        <th>Kapacita</th>
        <th>Viditelný</th>
      </tr>
    </thead>
    <tbody>
      {data!.allAkces.nodes.map((a) => <tr key={a.aId}>
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

export class EventListElement extends HTMLElement {
  connectedCallback() {
    ReactDOM.render(
      <ApolloProvider client={createClient()}><EventList /></ApolloProvider>,
      this
    );
  }
}
