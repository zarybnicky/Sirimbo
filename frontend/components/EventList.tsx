import * as React from 'react';
import { Checkbox, Menu, MenuItem, Button } from '@mui/material';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { Pagination } from '@mui/lab';
import { $, AkcesOrderBy, Selector } from 'lib/zeus';
import { useTypedQuery, useTypedMutation } from 'lib/zeus/apollo';
import { DateRange } from './DateRange';
import { scalars } from 'lib/apollo';
import { NextLinkComposed } from './Link';

export const EventListQuery = Selector('Query')({
  akces: [
    {
      first: $('limit', 'Int!'),
      offset: $('offset', 'Int!'),
      orderBy: [AkcesOrderBy.A_OD_DESC],
    },
    {
      nodes: {
        aDo: true,
        aId: true,
        aInfo: true,
        aDokumenty: true,
        aJmeno: true,
        aKapacita: true,
        aKde: true,
        aLock: true,
        aOd: true,
        aTimestamp: true,
        aVisible: true,
        akceItemsByAiIdRodic: [{}, {
          nodes: {
            aiId: true,
            userByAiUser: {
              uJmeno: true,
              uPrijmeni: true,
              uId: true,
            },
          },
          totalCount: true,
        }],
      },
      totalCount: true,
    },
  ],
});

export const ToggleEventVisible = Selector('Mutation')({
  updateAkce: [
    {
      input: {
        aId: $('id', 'BigInt!'),
        patch: {
          aVisible: $('visible', 'Boolean!')
        },
      },
    },
    {
      akce: {
        aId: true,
      },
    },
  ],
});

export function EventList() {
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(1);
  const { data, refetch } = useTypedQuery(EventListQuery, {
    scalars,
    apolloOptions: {
      variables: { limit, offset: (page - 1) * limit },
    },
  });
  const [toggleVisible] = useTypedMutation(ToggleEventVisible, {
    apolloOptions: {
      onCompleted: () => refetch(),
    },
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
      {(data?.akces?.nodes || []).map((a) => <tr key={a.aId}>
        <td>
          <PopupState variant="popover">
            {(popupState) => <React.Fragment>
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
            </React.Fragment>}
          </PopupState>
        </td>
        <td><DateRange from={a.aOd} to={a.aDo} /></td>
        <td>{a.akceItemsByAiIdRodic.totalCount || 0}/{a.aKapacita}</td>
        <td>
          <Checkbox checked={a.aVisible} onChange={() => toggleVisible({
            variables: { id: a.aId, visible: !a.aVisible },
          })} />
        </td>
      </tr>)}
    </tbody>
  </table>;

  return <React.Fragment>
    <NextLinkComposed href="/admin/akce/add" className="btn btn-primary">Přidat</NextLinkComposed>
    {list}
    <Pagination count={Math.ceil(total / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </React.Fragment>;
}
