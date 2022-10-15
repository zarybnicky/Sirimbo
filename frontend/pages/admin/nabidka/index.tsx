import * as React from 'react';
import { Checkbox, Pagination } from '@mui/material';
import { useAuth } from 'lib/data/use-auth';
import { DateRange } from 'components/DateRange';
import { NextLinkComposed } from 'components/Link';
import { useReservationListQuery, useToggleReservationVisibleMutation } from 'lib/graphql';
import { Dropdown } from 'components/Dropdown';

export default function ReservationAdminList() {
  const { user } = useAuth();
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(0);
  const { data, refetch } = useReservationListQuery({
    limit, offset: (page - 1) * limit,
  });
  const { mutate: toggleVisible } = useToggleReservationVisibleMutation({
    onSuccess: () => refetch(),
  });
  const total = data?.nabidkas?.totalCount || 0;

  const list = (!user || !total) ? null : <table>
    <thead>
      <tr>
        <th>Trenér</th>
        <th>Datum</th>
        <th>Viditelný</th>
      </tr>
    </thead>
    <tbody>
      {data?.nabidkas?.nodes?.map((a) => <tr key={a.nId}>
        <td>
          <Dropdown
            button={<>{a.userByNTrener?.uJmeno} {a.userByNTrener?.uPrijmeni}</>}
            options={[
              { title: 'Upravit', href: `/admin/nabidka/edit/${a.nId}` },
              { title: 'Upravit lekce', href: `/admin/nabidka/detail/${a.nId}` },
              { title: 'Duplikovat', href: `/admin/nabidka/duplicate/${a.nId}` },
              { title: 'Odstranit', href: `/admin/nabidka/remove/${a.nId}` },
            ]}
          />
        </td>
        <td><DateRange from={a.nOd} to={a.nDo} /></td>
        <td>
          <Checkbox checked={a.nVisible} onChange={() => toggleVisible({ id: a.nId, visible: !a.nVisible })} />
        </td>
      </tr>)}
    </tbody>
  </table>;

  return <>
    <NextLinkComposed href="/admin/nabidka/add" className="btn btn-primary">Nová nabídka</NextLinkComposed>
    {list}
    <Pagination count={Math.ceil(total / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </>;
}
