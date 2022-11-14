import * as React from 'react';
import format from 'date-fns/format';
import { Pagination, Card, CardContent } from '@mui/material';
import NavigateBeforeIcon from '@mui/icons-material/NavigateBefore';
import NavigateNextIcon from '@mui/icons-material/NavigateNext';
import { useAnnouncementListQuery } from 'lib/graphql';
import { HtmlView } from './HtmlView';

export function AnnouncementList() {
  const [limit] = React.useState(3);
  const [page, setPage] = React.useState(1);
  const { data } = useAnnouncementListQuery({ limit, offset: (page - 1) * limit });
  if (!data?.upozornenis) {
    // react-skeleton
    return null;
  }
  const nodes = data.upozornenis.nodes;
  const total = data.upozornenis.totalCount;
  const hasNext = total >= page * limit;
  const hasPrev = 0 < (page - 1) * limit;

  return <>
    <div className="flex items-center justify-center">
      {hasPrev && (
        <button className="button button-icon text-slate-500" onClick={() => setPage(page - 1)}>
          <NavigateBeforeIcon />
        </button>
      )}

      <span className="text-slate-500">
        {nodes.length > 0 ? format(new Date(nodes[0]!.upTimestampAdd), 'd. M. y') : ''}
        {' - '}
        {nodes.length > 0 ? format(new Date(nodes[nodes.length - 1]!.upTimestampAdd), 'd. M. y') : ''}
      </span>

      {hasNext && (
        <button className="button button-icon text-slate-500" onClick={() => setPage(page + 1)}>
          <NavigateNextIcon />
        </button>
      )}
    </div>
    {nodes.map((a) => <Card key={a.upId} className="mb-4">
      <CardContent>
        <h2 className="text-2xl mb-2">
          {a.upNadpis}
        </h2>
        <p className="text-slate-500 mb-4">
          {format(new Date(a.upTimestampAdd), 'd. M. y')} , {a.userByUpKdo?.uJmeno} {a.userByUpKdo?.uPrijmeni}
        </p>

        <HtmlView content={a.upText} />

        {a.upozorneniSkupiniesByUpsIdRodic?.nodes?.length <= 0 ? null : <div>
          {a.upozorneniSkupiniesByUpsIdRodic.nodes.map((g) =>
            <div className="w-3 h-3"
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
