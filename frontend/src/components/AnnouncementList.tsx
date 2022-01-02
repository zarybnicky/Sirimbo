import * as React from 'react';
import format from 'date-fns/format';
import { Box, IconButton, Card, CardContent, Typography } from '@material-ui/core';
import { Pagination } from '@material-ui/lab';
import { useTypedQuery } from '../zeus/apollo';
import { $, UpozornenisOrderBy, Selector } from '../zeus';

import NavigateBeforeIcon from '@material-ui/icons/NavigateBefore';
import NavigateNextIcon from '@material-ui/icons/NavigateNext';

const AnnouncementQuery = Selector('Query')({
  allUpozornenis: [
    { first: $`limit`, offset: $`offset`, orderBy: [UpozornenisOrderBy.UP_TIMESTAMP_ADD_DESC] },
    {
      totalCount: true,
      nodes: {
        upId: true,
        upKdo: true,
        upLock: true,
        upNadpis: true,
        upText: true,
        upTimestamp: true,
        upTimestampAdd: true,
        userByUpKdo: {
          uId: true,
          uJmeno: true,
          uPrijmeni: true,
        },
        upozorneniSkupiniesByUpsIdRodic: [{}, {
          nodes: {
            skupinyByUpsIdSkupina: {
              sName: true,
              sDescription: true,
              sColorText: true,
              sColorRgb: true,
            }
          }
        }],
      },
    },
  ],
});

export function AnnouncementList() {
  const [limit] = React.useState(5);
  const [page, setPage] = React.useState(1);
  const { data } = useTypedQuery(AnnouncementQuery, {
    variables: { limit, offset: (page - 1) * limit },
  });
  const nodes = data?.allUpozornenis?.nodes;
  const total = data?.allUpozornenis?.totalCount;
  if (nodes === undefined || total === undefined) {
    // react-skeleton
    return null;
  }
  const hasNext = total >= page * limit;
  const hasPrev = 0 < (page - 1) * limit;

  return <React.Fragment>
    <Box display='flex' alignItems='center' justifyContent="right">
      {hasPrev ? <IconButton onClick={() => setPage(page - 1)}><NavigateBeforeIcon /></IconButton> : null}
      <Typography color="textSecondary" component="span">
        {format(new Date(nodes[0].upTimestampAdd), 'd. M. y')}
        {' - '}
        {format(new Date(nodes[nodes.length - 1].upTimestampAdd), 'd. M. y')}
      </Typography>
      {hasNext ? <IconButton onClick={() => setPage(page + 1)}><NavigateNextIcon /></IconButton> : null}
    </Box>
    {nodes.map((a) => <Card key={a.upId} style={{ marginBottom: '1rem' }}>
      <CardContent>
        <Typography color="textSecondary">{format(new Date(a.upTimestampAdd), 'd. M. y')}</Typography>
        <Typography variant="h5" component="h2">{a.upNadpis}</Typography>
        <Typography color="textSecondary">{a.userByUpKdo?.uJmeno} {a.userByUpKdo?.uPrijmeni}</Typography>
        <div dangerouslySetInnerHTML={{ __html: a.upText }}></div>
        {a.upozorneniSkupiniesByUpsIdRodic?.nodes?.length <= 0 ? null : <div>
          {a.upozorneniSkupiniesByUpsIdRodic.nodes.map((g) =>
            <div className="box"
              key={g.skupinyByUpsIdSkupina?.sColorRgb}
              title={g.skupinyByUpsIdSkupina?.sName}
              style={{ backgroundColor: g.skupinyByUpsIdSkupina?.sColorRgb }}
            ></div>
          )}
        </div>}
      </CardContent>
    </Card>)}
    <Pagination count={Math.ceil(total / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </React.Fragment>;
}
