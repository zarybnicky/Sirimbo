import * as React from 'react';
import format from 'date-fns/format';
import { Button, Menu, MenuItem, Pagination } from '@mui/material';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { $, AktualitiesOrderBy, Selector } from 'lib/zeus';
import { useAuth } from 'lib/data/use-auth';
import { NextLinkComposed } from 'components/Link';
import { useTypedQuery } from 'lib/query';

export const ArticlesAdminQuery = Selector('Query')({
  aktualities: [
    {
      first: $('limit', 'Int!'),
      offset: $('offset', 'Int!'),
      orderBy: [AktualitiesOrderBy.AT_TIMESTAMP_ADD_DESC]
    },
    {
      nodes: {
        atFoto: true,
        atFotoMain: true,
        atId: true,
        atJmeno: true,
        atKdo: true,
        atPreview: true,
        atText: true,
        atTimestampAdd: true,
        atTimestamp: true,
      },
      totalCount: true,
    },
  ],
});

export function ArticleAdminList() {
  const { user } = useAuth();
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(1);
  const { data } = useTypedQuery(['articleAdmin'], ArticlesAdminQuery, {}, {
    variables: { limit, offset: (page - 1) * limit },
  });
  const total = data?.aktualities?.totalCount || 0;

  const list = (!user || !total) ? null : <table>
    <thead>
      <tr><th>Jméno</th><th>Přidáno</th></tr>
    </thead>
    <tbody>
      {data?.aktualities?.nodes?.map((a) => <tr key={a.atId}>
        <td>
          <PopupState variant="popover">
            {(popupState) => <>
              <Button {...bindTrigger(popupState)}>{a.atJmeno}</Button>
              <Menu {...bindMenu(popupState)}>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/aktuality/edit/${a.atId}`}>
                  Upravit
                </MenuItem>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/aktuality/foto/${a.atId}`}>
                  Upravit fotky
                </MenuItem>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/aktuality/remove/${a.atId}`}>
                  Odstranit
                </MenuItem>
              </Menu>
            </>}
          </PopupState>
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
