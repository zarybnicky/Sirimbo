import { ScheduleForm } from 'components/ScheduleForm';
import { ScheduleDocument } from 'lib/graphql/Schedule';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { fromSlugArray } from 'lib/slugify';
import type { NextPageWithLayout } from 'pages/_app';
import { useGqlQuery } from 'lib/query';
import { ScheduleList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = useGqlQuery(ScheduleDocument, { id }, { enabled: !!id, cacheTime: 0 });
  return (
    <div className="container mx-auto max-w-3xl mt-12 mb-8">
      {data && (
        <ScheduleForm data={data.rozpi || undefined} onSuccess={() => router.back()} />
      )}
    </div>
  );
};

Page.list = <ScheduleList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peRozpis, PermissionLevel.P_OWNED];
Page.staticTitle = 'Rozpisy';

export default Page;
