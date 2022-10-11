import * as React from 'react';
import format from 'date-fns/format';
import { Pagination, Box, IconButton, Card, CardContent, Typography } from '@mui/material';
import { $, UpozornenisOrderBy, Selector } from 'lib/zeus';
import NavigateBeforeIcon from '@mui/icons-material/NavigateBefore';
import NavigateNextIcon from '@mui/icons-material/NavigateNext';
import { useTypedQuery } from 'lib/query';

const AnnouncementQuery = Selector('Query')({
  upozornenis: [
    {
      first: $('limit', 'Int!'),
      offset: $('offset', 'Int!'),
      orderBy: [UpozornenisOrderBy.UP_TIMESTAMP_ADD_DESC],
    },
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
  const [limit] = React.useState(3);
  const [page, setPage] = React.useState(1);
  const { data } = useTypedQuery(['announcements', page], AnnouncementQuery, {}, {
    variables: { limit, offset: (page - 1) * limit },
  });

  const nodes = data?.upozornenis?.nodes;
  const total = data?.upozornenis?.totalCount;
  if (nodes === undefined || total === undefined) {
    // react-skeleton
    return null;
  }
  const hasNext = total >= page * limit;
  const hasPrev = 0 < (page - 1) * limit;
  return <>
    <Box display='flex' alignItems='center' justifyContent="right">
      {hasPrev ? <IconButton onClick={() => setPage(page - 1)}><NavigateBeforeIcon /></IconButton> : null}
      <Typography color="textSecondary" component="span">
        {nodes.length > 0 ? format(new Date(nodes[0]!.upTimestampAdd), 'd. M. y') : ''}
        {' - '}
        {nodes.length > 0 ? format(new Date(nodes[nodes.length - 1]!.upTimestampAdd), 'd. M. y') : ''}
      </Typography>
      {hasNext ? <IconButton onClick={() => setPage(page + 1)}><NavigateNextIcon /></IconButton> : null}
    </Box>
    {nodes.map((a) => <Card key={a.upId} style={{ marginBottom: '1rem' }}>
      <CardContent>
        <Typography color="textSecondary">
          {format(new Date(a.upTimestampAdd), 'd. M. y')}
        </Typography>
        <Typography variant="h5" component="h2">
          {a.upNadpis}
        </Typography>
        <Typography color="textSecondary">
          {a.userByUpKdo?.uJmeno} {a.userByUpKdo?.uPrijmeni}
        </Typography>
        <div dangerouslySetInnerHTML={{ __html: a.upText }}></div>
        {a.upozorneniSkupiniesByUpsIdRodic?.nodes?.length <= 0 ? null : <div>
          {a.upozorneniSkupiniesByUpsIdRodic.nodes.map((g) =>
            <div className="box"
              key={g.skupinyByUpsIdSkupina?.sColorRgb}
              title={g.skupinyByUpsIdSkupina?.sName}
              style={{ backgroundColor: g.skupinyByUpsIdSkupina?.sColorRgb }}
            />
          )}
        </div>}
      </CardContent>
    </Card>)}
    <Pagination count={Math.ceil(total / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </>;
}
