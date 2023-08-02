import * as React from 'react';
import { Pagination } from '@app/ui/Pagination';
import { DateEl } from './date';
import { Dropdown } from './dropdown';
import { ScheduleListDocument } from '@app/graphql/Schedule';
import { useQuery } from 'urql';
import { useAuth } from '@app/ui/use-auth';

export default function RozpisAdminList() {
  const [page, setPage] = React.useState(1);
  const { perms } = useAuth();

  const [{ data }] = useQuery({
    query: ScheduleListDocument,
    variables: { first: 30, offset: (page - 1) * 30 },
  });

  return (
    <>
      <a href="/admin/rozpis/add" className="btn btn-primary">
        Nový rozpis
      </a>
      {!data?.rozpis?.nodes.length ? null : (
        <table>
          <thead>
            <tr>
              <th>Trenér</th>
              <th>Datum</th>
              <th>Místo</th>
              <th>Viditelný</th>
            </tr>
          </thead>
          <tbody>
            {data!.rozpis.nodes
              .filter((a) => perms.canEditSchedule(a))
              .map((a) => (
                <tr key={a.id}>
                  <td>
                    <Dropdown
                      links={{
                        [`/admin/rozpis/edit/${a.id}`]: 'Upravit',
                        [`/admin/rozpis/detail/${a.id}`]: 'Upravit lekce',
                      }}
                    />
                    {a.userByRTrener?.uJmeno} {a.userByRTrener?.uPrijmeni}
                  </td>
                  <td>
                    <DateEl date={a.rDatum} />
                  </td>
                  <td>{a.rKde}</td>
                  <td>
                    <input type="checkbox" checked={a.rVisible} disabled />
                  </td>
                </tr>
              ))}
          </tbody>
        </table>
      )}

      {!!data?.rozpis?.totalCount && (
        <Pagination
          total={data.rozpis.totalCount}
          limit={30}
          page={page}
          setPage={setPage}
        />
      )}
    </>
  );
}
