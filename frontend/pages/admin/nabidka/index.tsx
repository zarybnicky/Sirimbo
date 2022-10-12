import * as React from 'react';
import { Checkbox, Menu, MenuItem, Button, Pagination } from '@mui/material';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { useAuth } from 'lib/data/use-auth';
import { DateRange } from 'components/DateRange';
import { NextLinkComposed } from 'components/Link';
import { useReservationListQuery, useToggleReservationVisibleMutation } from 'index';

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
          <PopupState variant="popover">
            {(popupState) => <>
              <Button {...bindTrigger(popupState)}>
                {a.userByNTrener?.uJmeno} {a.userByNTrener?.uPrijmeni}
              </Button>
              <Menu {...bindMenu(popupState)}>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/nabidka/edit/${a.nId}`}>
                  Upravit
                </MenuItem>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/nabidka/detail/${a.nId}`}>
                  Upravit lekce
                </MenuItem>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/nabidka/duplicate/${a.nId}`}>
                  Duplikovat
                </MenuItem>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/nabidka/remove/${a.nId}`}>
                  Odstranit
                </MenuItem>
              </Menu>
            </>}
          </PopupState>
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
