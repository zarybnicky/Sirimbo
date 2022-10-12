import * as React from 'react';
import { Checkbox, Menu, MenuItem, Button, Pagination } from '@mui/material';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { DateRange } from 'components/DateRange';
import { NextLinkComposed } from 'components/Link';
import { useEventListQuery, useToggleEventVisibleMutation } from 'index';

export default function AdminEventList() {
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
          <PopupState variant="popover">
            {(popupState) => <>
              <Button {...bindTrigger(popupState)}>{a.aJmeno}</Button>
              <Menu {...bindMenu(popupState)}>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/akce/edit/${a.aId}`}>
                  Upravit
                </MenuItem>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/akce/detail/${a.aId}`}>
                  Upravit účastníky
                </MenuItem>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/akce/dokumenty/${a.aId}`}>
                  Upravit dokumenty
                </MenuItem>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/akce/remove/${a.aId}`}>
                  Odstranit
                </MenuItem>
              </Menu>
            </>}
          </PopupState>
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
