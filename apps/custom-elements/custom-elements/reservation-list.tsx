import React from 'react';
import { Pagination } from '@app/ui/Pagination';
import { DateRange } from './date';
import { Dropdown } from './dropdown';
import { useQuery } from 'urql';
import { ReservationListDocument } from '@app/graphql/Reservation';
import { useAuth } from '@app/ui/use-auth';

export default function ReservationAdminList() {
  const [page, setPage] = React.useState(1);
  const { perms } = useAuth();

  const [{ data }] = useQuery({
    query: ReservationListDocument,
    variables: { first: 10, offset: (page - 1) * 10 },
  });

  return (
    <>
      <a href="/admin/nabidka/add" className="btn btn-primary">
        Nová nabídka
      </a>
      {!data?.nabidkas?.nodes.length ? null : (
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
              .filter((a) => perms.canEditReservation(a))
              .map((a) => (
                <tr key={a.id}>
                  <td>
                    <Dropdown
                      links={{
                        [`/admin/nabidka/edit/${a.id}`]: 'Upravit',
                        [`/admin/nabidka/detail/${a.id}`]: 'Upravit lekce',
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
                      disabled
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
