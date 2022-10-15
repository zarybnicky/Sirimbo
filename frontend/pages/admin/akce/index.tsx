import * as React from 'react';
import { Checkbox, Pagination } from '@mui/material';
import { DateRange } from 'components/DateRange';
import { NextLinkComposed } from 'components/Link';
import { useEventListQuery, useToggleEventVisibleMutation } from 'lib/graphql';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { Dropdown } from 'components/Dropdown';

export default function AdminEventList() {
  useRequireUserLoggedIn();
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(1);
  const { data, refetch } = useEventListQuery({
    limit, offset: (page - 1) * limit,
  });
  const { mutate: toggleVisible } = useToggleEventVisibleMutation({
    onSuccess: () => refetch(),
  });
  const total = data?.akces?.totalCount || 0;

  const list = !total ? null : <table>
    <thead>
      <tr>
        <th>Jméno</th>
        <th>Datum</th>
        <th>Kapacita</th>
        <th>Viditelný</th>
      </tr>
    </thead>
    <tbody>
      {data?.akces?.nodes?.map((a) => <tr key={a.aId}>
        <td>
          <Dropdown
            button={<>{a.aJmeno}</>}
            options={[
              { title: 'Upravit', href: `/admin/rozpis/edit/${a.aId}` },
              { title: 'Upravit účastníky', href: `/admin/rozpis/detail/${a.aId}` },
              { title: 'Upravit dokumenty', href: `/admin/rozpis/detail/${a.aId}` },
              { title: 'Odstranit', href: `/admin/rozpis/remove/${a.aId}` },
            ]}
          />
        </td>
        <td><DateRange from={a.aOd} to={a.aDo} /></td>
        <td>{a.akceItemsByAiIdRodic.totalCount || 0}/{a.aKapacita}</td>
        <td>
          <Checkbox checked={a.aVisible} onChange={() => toggleVisible({ id: a.aId, visible: !a.aVisible })} />
        </td>
      </tr>)}
    </tbody>
  </table>;

  return <>
    <NextLinkComposed href="/admin/akce/add" className="btn btn-primary">Přidat</NextLinkComposed>
    {list}
    <Pagination count={Math.ceil(total / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </>;
}
