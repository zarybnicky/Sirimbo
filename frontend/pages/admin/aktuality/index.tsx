import * as React from 'react';
import format from 'date-fns/format';
import { Pagination } from '@mui/material';
import { useAuth } from 'lib/data/use-auth';
import { NextLinkComposed } from 'components/Link';
import { useArticlesQuery } from 'lib/graphql';
import { Dropdown } from 'components/Dropdown';

export function ArticleAdminList() {
  const { user } = useAuth();
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(1);
  const { data } = useArticlesQuery({
    limit, offset: (page - 1) * limit,
  });
  const total = data?.aktualities?.totalCount || 0;

  const list = (!user || !total) ? null : <table>
    <thead>
      <tr><th>Jméno</th><th>Přidáno</th></tr>
    </thead>
    <tbody>
      {data?.aktualities?.nodes?.map((a) => <tr key={a.atId}>
        <td>
          <Dropdown
            button={<>a.atJmeno</>}
            options={[
              { title: 'Upravit', href: `/admin/aktuality/edit/${a.atId}` },
              { title: 'Upravit fotky', href: `/admin/aktuality/foto/${a.atId}` },
              { title: 'Odstranit', href: `/admin/aktuality/remove/${a.atId}` },
            ]}
          />
        </td>
        <td>{a.atTimestampAdd && format(new Date(a.atTimestampAdd), 'd. M. y')}</td>
      </tr>)}
    </tbody>
  </table >;

  return <>
    <NextLinkComposed href="/admin/aktuality/add" className="btn btn-primary">Nový článek</NextLinkComposed>
    {list}
    <Pagination count={Math.ceil(total / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </>;
}
