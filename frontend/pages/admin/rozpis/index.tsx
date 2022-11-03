import * as React from 'react';
import format from 'date-fns/format';
import { Pagination, Checkbox, Container } from '@mui/material';
import { NextLinkComposed } from 'components/Link';
import { useScheduleListQuery, useToggleScheduleVisibleMutation } from 'lib/graphql';
import { Dropdown } from 'components/Dropdown';
import { useRouter } from 'next/router';
import { useConfirm } from 'material-ui-confirm';
import { useSnackbar } from 'notistack';

export default function RozpisAdminList() {
  const router = useRouter();
  const confirm = useConfirm();
  const { enqueueSnackbar } = useSnackbar();
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(0);
  const { data, refetch } = useScheduleListQuery({ limit, offset: page * limit });
  const { mutateAsync: toggleVisible } = useToggleScheduleVisibleMutation({
    onSuccess: () => refetch(),
  });

  const rowCount = data?.rozpis?.totalCount || 0;
  const [rowCountState, setRowCountState] = React.useState(rowCount);
  React.useEffect(() => {
    setRowCountState((prev) => rowCount !== undefined ? rowCount : prev);
  }, [rowCount]);

  return <Container maxWidth="lg" style={{ padding: '4rem 0 6rem' }}>
    <NextLinkComposed href="/admin/rozpis/add" className="btn btn-primary">Nový rozpis</NextLinkComposed>
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
        {data?.rozpis?.nodes?.map((a) => <tr key={a.rId}>
          <td>
            <Dropdown
              button={<>{a.userByRTrener?.uJmeno} {a.userByRTrener?.uPrijmeni}</>}
              options={[
                { title: 'Upravit', href: `/admin/rozpis/edit/${a.rId}` },
                { title: 'Upravit lekce', href: `/admin/rozpis/detail/${a.rId}` },
                { title: 'Duplikovat', href: `/admin/rozpis/duplicate/${a.rId}` },
                { title: 'Odstranit', href: `/admin/rozpis/remove/${a.rId}` },
              ]}
            />
          </td>
          <td>{format(new Date(a.rDatum), 'd. M. y')}</td>
          <td>{a.rKde}</td>
          <td>
            <Checkbox checked={a.rVisible || false} onChange={() => toggleVisible({
              id: a.rId, visible: !a.rVisible,
            })} />
          </td>
        </tr>)}
      </tbody>
    </table>
    <Pagination count={Math.ceil(total / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </Container>;
}
