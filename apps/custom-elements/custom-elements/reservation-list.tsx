import React from 'react';
import { Pagination } from '@app/ui/Pagination';
import { DateRange } from './date';
import { Dropdown } from './dropdown';
import { useQuery, useMutation } from 'urql';
import { CurrentUserDocument } from '@app/graphql/CurrentUser';
import {
  ToggleReservationVisibleDocument,
  ReservationListDocument,
} from '@app/graphql/Reservation';

export default function ReservationAdminList() {
  const [page, setPage] = React.useState(1);
  const [{ data: user }] = useQuery({
    query: CurrentUserDocument,
  });
  const [{ data }, refetch] = useQuery({
    query: ReservationListDocument,
    variables: { first: 10, offset: (page - 1) * 10 },
  });
  const [_, toggleVisible] = useMutation(ToggleReservationVisibleDocument);

  return (
    <>
      <a href="/admin/nabidka/add" className="btn btn-primary">
        Nová nabídka
      </a>
      {!user || !data?.nabidkas?.nodes.length ? null : (
        <table>
          <thead>
            <tr>
              <th>Trenér</th>
              <th>Datum</th>
              <th>Viditelný</th>
            </tr>
          </thead>
          <tbody>
            {data!.nabidkas?.nodes
              .filter(
                (a) =>
                  16 <= (user.getCurrentUser?.permissionByUGroup?.peNabidka || 0) ||
                  a.nTrener == user.getCurrentUser?.id,
              )
              .map((a) => (
                <tr key={a.id}>
                  <td>
                    <Dropdown
                      links={{
                        [`/admin/nabidka/edit/${a.id}`]: 'Upravit',
                        [`/admin/nabidka/detail/${a.id}`]: 'Upravit lekce',
                        [`/admin/nabidka/duplicate/${a.id}`]: 'Duplikovat',
                        [`/admin/nabidka/remove/${a.id}`]: 'Odstranit',
                      }}
                    />
                    {a.userByNTrener?.uJmeno} {a.userByNTrener?.uPrijmeni}
                  </td>
                  <td>
                    <DateRange from={a.nOd} to={a.nDo} />
                  </td>
                  <td>
                    <input
                      type="checkbox"
                      checked={a.nVisible}
                      onChange={async () => {
                        await toggleVisible({ id: a.id, visible: !a.nVisible });
                        refetch();
                      }}
                    />
                  </td>
                </tr>
              ))}
          </tbody>
        </table>
      )}

      {!!data?.nabidkas?.totalCount && (
        <Pagination
          total={data.nabidkas.totalCount}
          limit={10}
          page={page}
          setPage={setPage}
        />
      )}
    </>
  );
}