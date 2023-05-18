import { List } from 'components/layout/List';
import { fullDateFormatter } from 'lib/format-date';
import { AnnouncementListDocument } from 'lib/graphql/Announcement';
import { useGqlQuery } from 'lib/query';
import { fromSlugArray } from 'lib/slugify';
import { useRouter } from 'next/router';
import React from 'react';
import { Plus } from 'react-feather';
import { CohortColorBoxes } from './CohortColorBox';
import { FuzzyList } from './FuzzyList';
import { Pagination } from './Pagination';

export function AnnouncementList() {
  const router = useRouter();
  const [limit] = React.useState(12);
  const [page, setPage] = React.useState(1);
  const { data } = useGqlQuery(AnnouncementListDocument, {
    limit,
    offset: (page - 1) * limit,
  });

  const id = fromSlugArray(router.query.id);
  const total = data?.upozornenis?.totalCount || 0;

  return (
    <List>
      <List.TitleBar title="Upozornění">
        <List.TitleButton
          active={router.asPath.endsWith('add')}
          icon={Plus}
          href="/admin/nastenka/add"
        >
          Nový příspevek
        </List.TitleButton>
      </List.TitleBar>

      <FuzzyList
        data={data?.upozornenis?.nodes || []}
        fields={['id', 'upNadpis']}
        renderItem={(n, item) => (
          <List.Item
            key={item.id}
            active={id === item.id}
            href={{ pathname: '/admin/nastenka/[id]', query: { id: item.id } }}
            title={item.upNadpis}
            subtitle={
              <div className="flex flex-wrap justify-between items-baseline gap-4">
                <div>
                  {[
                    item.userByUpKdo &&
                      `${item.userByUpKdo.uJmeno} ${item.userByUpKdo.uPrijmeni}`,
                    fullDateFormatter.format(new Date(item.upTimestampAdd)),
                  ]
                    .filter(Boolean)
                    .join(', ')}
                </div>
                <CohortColorBoxes
                  items={item.upozorneniSkupiniesByUpsIdRodic?.nodes.map(
                    (x) => x.skupinyByUpsIdSkupina,
                  )}
                />
              </div>
            }
          />
        )}
      />

      <Pagination {...{ total, limit, page, setPage }} />
    </List>
  );
}
