import * as React from 'react';
import { format } from 'date-fns';
import { ChevronLeft, ChevronRight } from 'react-feather';
import { useAnnouncementListQuery } from 'lib/graphql/Announcement';
import { HtmlView } from './HtmlView';
import { Card } from 'components/Card';
import { Pagination } from './Pagination';

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
          <ChevronLeft />
        </button>
      )}

      <span className="text-slate-500">
        {nodes.length > 0 ? format(new Date(nodes[0]!.upTimestampAdd), 'd. M. y') : ''}
        {' - '}
        {nodes.length > 0 ? format(new Date(nodes[nodes.length - 1]!.upTimestampAdd), 'd. M. y') : ''}
      </span>

      {hasNext && (
        <button className="button button-icon text-slate-500" onClick={() => setPage(page + 1)}>
          <ChevronRight />
        </button>
      )}
    </div>

    {nodes.map((a) => <Card key={a.upId} className="mb-4">
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
    </Card>)}

    <Pagination {...{ total, limit, page, setPage }} />
  </>;
}
