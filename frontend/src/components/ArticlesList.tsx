import * as React from 'react';
import format from 'date-fns/format';
import { Link } from 'react-router-dom';
import { Button, Menu, MenuItem } from '@material-ui/core';
import { Pagination } from '@material-ui/lab';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { $, AktualitiesOrderBy, Selector } from '../zeus';
import { useTypedQuery } from '../zeus/apollo';
import { useAuth } from '../data/use-auth';

export const ArticlesAdminQuery = Selector('Query')({
  aktualities: [
    { first: $`limit`, offset: $`offset`, orderBy: [AktualitiesOrderBy.AT_TIMESTAMP_ADD_DESC] },
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
  const { data } = useTypedQuery(ArticlesAdminQuery, {
    variables: { limit, offset: (page - 1) * limit },
  });
  const total = data?.aktualities?.totalCount || 0;

  const list = (!user || !total) ? null : <table>
    <thead>
      <tr><th>Jméno</th><th>Přidáno</th></tr>
    </thead>
    <tbody>
      {data!.aktualities?.nodes.map((a) => <tr key={a.atId}>
        <td>
          <PopupState variant="popover">
            {(popupState) => <React.Fragment>
              <Button {...bindTrigger(popupState)}>{a.atJmeno}</Button>
              <Menu {...bindMenu(popupState)} getContentAnchorEl={null}>
                <MenuItem button onClick={popupState.close} component={Link} to={`/admin/aktuality/edit/${a.atId}`}>
                  Upravit
                </MenuItem>
                <MenuItem button onClick={popupState.close} component={Link} to={`/admin/aktuality/foto/${a.atId}`}>
                  Upravit fotky
                </MenuItem>
                <MenuItem button onClick={popupState.close} component={Link} to={`/admin/aktuality/remove/${a.atId}`}>
                  Odstranit
                </MenuItem>
              </Menu>
            </React.Fragment>}
          </PopupState>
        </td>
        <td>{format(new Date(a.atTimestampAdd), 'd. M. y')}</td>
      </tr>)}
    </tbody>
  </table >;

  return <React.Fragment>
    <a href="/admin/aktuality/add" className="btn btn-primary">Nový článek</a>
    {list}
    <Pagination count={Math.ceil(total / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </React.Fragment>;
}