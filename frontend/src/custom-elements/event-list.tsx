import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import { ApolloProvider, HttpLink, ApolloClient, InMemoryCache } from '@apollo/client';
import Form from 'react-bootstrap/Form';
import { Pagination } from './pagination';
import { DateRange } from './date';
import { Dropdown } from './dropdown';
import { $, EventsOrderBy, Selector } from '../zeus';
import { useTypedQuery, useTypedMutation } from '../zeus/apollo';

export const AkceList = Selector('Query')({
  events: [
    { first: $`limit`, offset: $`offset`, orderBy: [EventsOrderBy.SINCE_ASC] },
    {
      nodes: {
        id: true,
        since: true,
        until: true,
        description: true,
        title: true,
        filesLegacy: true,
        capacity: true,
        locationText: true,
        updatedAt: true,
        isLocked: true,
        isVisible: true,
        attendeeUsers: [{}, {
          nodes: {
            id: true,
            user: {
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
  updateEvent: [
    { input: { id: $`id`, patch: { isVisible: $`visible` } } },
    {
      event: {
        id: true,
      },
    },
  ],
});

export function EventList() {
  const [limit] = useState(30);
  const [offset, setOffset] = useState(0);
  const [total, setTotal] = useState(0);
  const { data, refetch } = useTypedQuery(AkceList, {
    variables: { limit, offset },
    onCompleted: (data) => {
      const total = data.events?.totalCount;
      total && setTotal(total);
    },
  });
  const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);
  const [toggleVisible] = useTypedMutation(ToggleVisible, {
    onCompleted: () => refetch(),
  });

  const list = !data?.events?.nodes.length ? null : <table>
    <thead>
      <tr>
        <th>Jméno</th>
        <th>Datum</th>
        <th>Kapacita</th>
        <th>Viditelný</th>
      </tr>
    </thead>
    <tbody>
      {data!.events.nodes.map((a) => <tr key={a.id}>
        <td>
          <Dropdown links={{
            [`/admin/akce/edit/${a.id}`]: "Upravit",
            [`/admin/akce/detail/${a.id}`]: "Upravit účastníky",
            [`/admin/akce/dokumenty/${a.id}`]: "Upravit dokumenty",
            [`/admin/akce/remove/${a.id}`]: "Odstranit",
          }} />
          {a.title}
        </td>
        <td><DateRange from={a.since} to={a.until} /></td>
        <td>{a.attendeeUsers.totalCount || 0}/{a.capacity}</td>
        <td>
          <Form.Check checked={a.isVisible} onChange={() => toggleVisible({
            variables: { id: a.id, visible: !a.isVisible },
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
