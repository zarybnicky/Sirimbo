import * as React from 'react';
import format from 'date-fns/format';
import { Pagination } from '@material-ui/lab';
import { useTypedQuery } from '../zeus/apollo';
import { $, UpozornenisOrderBy, Selector } from '../zeus';

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
  const [limit] = React.useState(10);
  const [page, setPage] = React.useState(1);
  const [total, setTotal] = React.useState(0);
  const { data } = useTypedQuery(AnnouncementQuery, {
    variables: { limit, offset: (page - 1) * limit },
    onCompleted: (data) => {
      const total = data.allUpozornenis?.totalCount;
      total && setTotal(total);
    },
  });

  return <React.Fragment>
    {!data?.allUpozornenis ? null : data.allUpozornenis.nodes.map((a) => <React.Fragment>
      <div className="row">
        <div className="col h3">{a.upNadpis}</div>
        <div className="col-12 col-md-4 text-right h6">
          {a.userByUpKdo?.uJmeno} {a.userByUpKdo?.uPrijmeni}{', '}
          {format(new Date(a.upTimestampAdd), 'd. M. y')}
        </div>
        {a.upozorneniSkupiniesByUpsIdRodic?.nodes?.length <= 0 ? null : <div className="col-12">
          <span className="little">skupiny:&nbsp;</span>
          {a.upozorneniSkupiniesByUpsIdRodic.nodes.map((g) =>
            <div className="box"
              key={g.skupinyByUpsIdSkupina?.sColorRgb}
              title={g.skupinyByUpsIdSkupina?.sName}
              style={{ backgroundColor: g.skupinyByUpsIdSkupina?.sColorRgb }}
            ></div>
          )}
        </div>}
      </div>
      <div style={{ paddingTop: '8px' }} dangerouslySetInnerHTML={{ __html: a.upText }}></div>
      <hr />
    </React.Fragment>)}
    <Pagination count={Math.ceil(total / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </React.Fragment>;
}
