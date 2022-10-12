import * as React from 'react';
import format from 'date-fns/format';
import { Pagination, Checkbox, Menu, MenuItem, Button } from '@mui/material';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { useAuth } from 'lib/data/use-auth';
import { NextLinkComposed } from 'components/Link';
import { useScheduleListQuery, useToggleScheduleVisibleMutation } from 'index';

export default function RozpisAdminList() {
  const { user } = useAuth();
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(1);
  const { data, refetch } = useScheduleListQuery({ limit, offset: (page - 1) * limit });
  const { mutateAsync: toggleVisible } = useToggleScheduleVisibleMutation({
    onSuccess: () => refetch(),
  });
  const total = data?.rozpis?.totalCount || 0;

  const list = (!user || !total) ? null : <table>
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
          <PopupState variant="popover">
            {(popupState) => <>
              <Button {...bindTrigger(popupState)}>
                {a.userByRTrener?.uJmeno} {a.userByRTrener?.uPrijmeni}
              </Button>
              <Menu {...bindMenu(popupState)}>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/rozpis/edit/${a.rId}`}>
                  Upravit
                </MenuItem>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/rozpis/detail/${a.rId}`}>
                  Upravit lekce
                </MenuItem>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/rozpis/duplicate/${a.rId}`}>
                  Duplikovat
                </MenuItem>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/rozpis/remove/${a.rId}`}>
                  Odstranit
                </MenuItem>
              </Menu>
            </>}
          </PopupState>
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
  </table >;

  return <>
    <NextLinkComposed href="/admin/rozpis/add" className="btn btn-primary">Nový rozpis</NextLinkComposed>
    {list}
    <Pagination count={Math.ceil(total / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </>;
}
