import React from 'react';
import { Pagination } from '@app/ui/Pagination';
import { DateRange } from './date';
import { Dropdown } from './dropdown';
import { useQuery, useMutation } from 'urql';
import { ToggleEventVisibleDocument, EventListDocument } from '@app/graphql/Event';

export default function EventList() {
  const [page, setPage] = React.useState(1);
  const [{ data }, refetch] = useQuery({
    query: EventListDocument,
    variables: { first: 10, offset: (page - 1) * 10 },
  });
  const [_, toggleVisible] = useMutation(ToggleEventVisibleDocument);

  return (
    <>
      <a href="/akce/add" className="btn btn-primary">
        Přidat
      </a>
      {!data?.events?.nodes.length ? null : (
        <table>
          <thead>
            <tr>
              <th>Jméno</th>
              <th>Datum</th>
              <th>Kapacita</th>
              <th>Viditelný</th>
            </tr>
          </thead>
          <tbody>
            {data!.events.nodes.map((a) => (
              <tr key={a.id}>
                <td>
                  <Dropdown
                    links={{
                      [`/akce/edit/${a.id}`]: 'Upravit',
                      [`/akce/detail/${a.id}`]: 'Upravit účastníky',
                      [`/akce/remove/${a.id}`]: 'Odstranit',
                    }}
                  />
                  {a.name}
                </td>
                <td>
                  <DateRange from={a.since} to={a.until} />
                </td>
                <td>
                  {parseInt(a.capacity) - (a.remainingPersonSpots || 0)}/{a.capacity}
                </td>
                <td>
                  <input
                    type="checkbox"
                    checked={a.isVisible}
                    onChange={async () => {
                      await toggleVisible({ id: a.id, visible: !a.isVisible });
                      refetch();
                    }}
                  />
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      )}

      {!!data?.events?.totalCount && (
        <Pagination
          total={data.events.totalCount}
          limit={10}
          page={page}
          setPage={setPage}
        />
      )}
    </>
  );
}
