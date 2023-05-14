import { useCohortListQuery } from 'lib/graphql/Cohorts';
import { Plus } from 'react-feather';
import { useRouter } from 'next/router';
import { List } from 'components/layout/List';
import { fromSlugArray } from 'lib/slugify';

export const CohortsList = () => {
  const router = useRouter();
  const { data } = useCohortListQuery();
  const id = fromSlugArray(router.query.id);

  return (
    <List>
      <List.TitleBar title="Skupiny">
        <List.TitleButton
          active={router.asPath.endsWith('add')}
          icon={Plus}
          href="/admin/skupiny/add"
        >
          Nová skupina
        </List.TitleButton>
      </List.TitleBar>

      <List.Scroll>
        {data?.skupinies?.nodes?.map((item) => (
          <List.Item
            key={item.id}
            active={id === item.id}
            href={{ pathname: '/admin/skupiny/[id]', query: { id: item.id } }}
            title={item.sName}
            className="pl-6"
            subtitle={[!item.sVisible && 'Skrytá', item.sLocation]
              .filter(Boolean)
              .join(', ')}
          >
            <div
              className="absolute rounded-l-lg w-4 shadow-sm top-0 bottom-0 left-0"
              style={{ backgroundColor: item.sColorRgb }}
            />
          </List.Item>
        ))}
      </List.Scroll>
    </List>
  );
};
