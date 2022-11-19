import { useAnnouncementListQuery } from "lib/graphql/Announcement";
import { Plus } from 'react-feather';
import { useRouter } from "next/router";
import { List } from "components/layout/List";
import { formatFullDate } from "lib/format-date";
import React from "react";
import { Pagination } from "./Pagination";

export function AnnouncementList() {
  const router = useRouter();
  const [limit] = React.useState(12);
  const [page, setPage] = React.useState(1);
  const { data } = useAnnouncementListQuery({ limit, offset: (page - 1) * limit });

  const { id } = router.query;
  const active = id ? id as string : null;
  const total = data?.upozornenis?.totalCount || 0;

  return <List>
    <List.TitleBar title="Upozornění">
      <List.TitleButton active={router.asPath.endsWith('add')} icon={Plus} href="/admin/nastenka/add">
        Nový příspevek
      </List.TitleButton>
    </List.TitleBar>

    <List.Scroll>
      {data?.upozornenis?.nodes?.map((item) => (
        <List.Item
          key={item.id}
          active={active === item.id} href={`/admin/nastenka/${item.id}`}
          title={item.upNadpis}
          subtitle={<div className="flex justify-between flex-wrap">
            <div>
              {[
                item.userByUpKdo && `${item.userByUpKdo.uJmeno} ${item.userByUpKdo.uPrijmeni}`,
                formatFullDate(new Date(item.upTimestampAdd)),
              ].filter(Boolean).join(', ')}
            </div>
            <div className="inline-flex gap-1 ml-3">
              {item.upozorneniSkupiniesByUpsIdRodic?.nodes?.length <= 0 ? null : (
                item.upozorneniSkupiniesByUpsIdRodic.nodes.map((g) =>
                  <div className="w-3.5 h-3.5"
                    key={g.skupinyByUpsIdSkupina?.id}
                    title={g.skupinyByUpsIdSkupina?.sName}
                    style={{ backgroundColor: g.skupinyByUpsIdSkupina?.sColorRgb }}
                  />
                )
              )}
            </div>
          </div>}
        />
      ))}
    </List.Scroll>

    <Pagination {...{ total, limit, page, setPage }} />
  </List>;
}
