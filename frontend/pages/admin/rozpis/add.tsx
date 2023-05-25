import { ScheduleForm } from 'components/ScheduleForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { ScheduleList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  return (
    <div className="container mx-auto max-w-3xl mt-12 mb-8">
      <ScheduleForm onSuccess={() => router.back()} />
    </div>
  );
};

Page.list = <ScheduleList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peRozpis, PermissionLevel.P_OWNED];
Page.staticTitle = "Rozpisy";

export default Page;
