import * as React from 'react';
import { ChevronLeft, ChevronRight } from 'react-feather';
import { useAnnouncementListQuery } from 'lib/graphql/Announcement';
import { Card } from 'components/Card';
import { Pagination } from './Pagination';
import { formatFullDate, fullDateFormatter } from 'lib/format-date';
import { RichTextView } from './RichTextView';

export function MyAnnouncements() {
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

  return <div className="flex flex-col items-center">
    <h4 className="text-2xl tracking-wide">Nástěnka</h4>
    <div className="flex items-center mb-4">
      {hasPrev ? (
        <button className="button button-icon text-stone-500" onClick={() => setPage(page - 1)}>
          <ChevronLeft />
        </button>
      ) : (
        <button className="button button-icon text-stone-300">
          <ChevronLeft />
        </button>
      )}

      <span className="text-stone-500">
        {nodes.length > 0 ? fullDateFormatter.formatRange(new Date(nodes[0]!.upTimestampAdd), new Date(nodes[nodes.length - 1]!.upTimestampAdd)) : ''}
      </span>

      {hasNext ? (
        <button className="button button-icon text-stone-500" onClick={() => setPage(page + 1)}>
          <ChevronRight />
        </button>
      ) : (
        <button className="button button-icon text-stone-300">
          <ChevronRight />
        </button>
      )}
    </div>

    {nodes.map((a) => <Card key={a.id} className="mb-4">
      <h2 className="text-2xl">{a.upNadpis}</h2>
      <div className="text-stone-500 flex justify-between flex-wrap mb-4">
        <div>
          {[
            formatFullDate(new Date(a.upTimestampAdd)),
            a.userByUpKdo && `${a.userByUpKdo?.uJmeno} ${a.userByUpKdo?.uPrijmeni}`,
          ].filter(Boolean).join(', ')}
        </div>
        {a.upozorneniSkupiniesByUpsIdRodic?.nodes?.length <= 0 ? null : (
          <div className="flex gap-1">
            {a.upozorneniSkupiniesByUpsIdRodic.nodes.map((g) =>
              <div className="w-3 h-3"
                key={g.skupinyByUpsIdSkupina?.sColorRgb}
                title={g.skupinyByUpsIdSkupina?.sName}
                style={{ backgroundColor: g.skupinyByUpsIdSkupina?.sColorRgb }}
              />
            )}
          </div>
        )}
      </div>

      <RichTextView value={a.upText} />
    </Card>)}

    <Pagination {...{ total, limit, page, setPage }} />
  </div>;
}
