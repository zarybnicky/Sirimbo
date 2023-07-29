import React from 'react';
import { DateEl } from './date';
import { Pagination } from '@app/ui/Pagination';
import { useQuery } from 'urql';
import { MyAnnouncementsDocument } from '@app/graphql/Announcement';

export default function AnnouncementList() {
  const [page, setPage] = React.useState(1);
  const [{ data }] = useQuery({
    query: MyAnnouncementsDocument,
    variables: { first: 10, offset: (page - 1) * 10 },
  });

  return (
    <>
      {(data?.myAnnouncements?.nodes || []).map((a) => (
        <>
          <div className="row">
            <div className="col h3">{a.upNadpis}</div>
            <div className="col-12 col-md-4 text-right h6">
              {a.userByUpKdo?.uJmeno} {a.userByUpKdo?.uPrijmeni}
              {', '}
              <DateEl date={a.upTimestampAdd} />
            </div>
            {a.upozorneniSkupiniesByUpsIdRodic?.nodes?.length <= 0 ? null : (
              <div className="col-12">
                <span className="little">skupiny:&nbsp;</span>
                {a.upozorneniSkupiniesByUpsIdRodic.nodes.map((g) => (
                  <div
                    className="box"
                    key={g.skupinyByUpsIdSkupina?.sColorRgb}
                    title={g.skupinyByUpsIdSkupina?.sName}
                    style={{ backgroundColor: g.skupinyByUpsIdSkupina?.sColorRgb }}
                  ></div>
                ))}
              </div>
            )}
          </div>
          <div
            style={{ paddingTop: '8px' }}
            dangerouslySetInnerHTML={{ __html: a.upText }}
          />
          <hr />
        </>
      ))}

      {!!data?.myAnnouncements?.totalCount && (
        <Pagination
          total={data.myAnnouncements.totalCount}
          limit={10}
          page={page}
          setPage={setPage}
        />
      )}
    </>
  );
}
