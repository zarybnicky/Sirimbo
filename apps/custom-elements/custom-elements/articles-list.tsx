import React from 'react';
import { Pagination } from '@app/ui/Pagination';
import { DateEl } from './date';
import { Dropdown } from './dropdown';
import { useQuery } from 'urql';
import { CurrentUserDocument } from '@app/graphql/CurrentUser';
import { ArticlesDocument } from '@app/graphql/Articles';

export default function ArticleAdminList() {
  const [page, setPage] = React.useState(1);
  const [{ data: user }] = useQuery({
    query: CurrentUserDocument,
  });
  const [{ data }] = useQuery({
    query: ArticlesDocument,
    variables: { first: 30, offset: (page - 1) * 30 },
  });

  return (
    <>
      <a href="/admin/aktuality/add" className="btn btn-primary">
        Nový článek
      </a>
      {!user || !data?.aktualities?.nodes.length ? null : (
        <table>
          <thead>
            <tr>
              <th>Jméno</th>
              <th>Přidáno</th>
            </tr>
          </thead>
          <tbody>
            {data.aktualities?.nodes
              .filter(
                (a) =>
                  16 <= (user.getCurrentUser?.permissionByUGroup?.peAktuality || 0) ||
                  a.atKdo == user.getCurrentUser?.id,
              )
              .map((a) => (
                <tr key={a.id}>
                  <td>
                    <Dropdown
                      links={{
                        [`/admin/aktuality/edit/${a.id}`]: 'Upravit',
                        [`/admin/aktuality/foto/${a.id}`]: 'Upravit fotky',
                        [`/admin/aktuality/remove/${a.id}`]: 'Odstranit',
                      }}
                    />
                    {a.atJmeno}
                  </td>
                  <td>
                    <DateEl date={a.atTimestampAdd ?? ''} />
                  </td>
                </tr>
              ))}
          </tbody>
        </table>
      )}

      {!!data?.aktualities?.totalCount && (
        <Pagination
          total={data.aktualities.totalCount}
          limit={30}
          page={page}
          setPage={setPage}
        />
      )}
    </>
  );
}
