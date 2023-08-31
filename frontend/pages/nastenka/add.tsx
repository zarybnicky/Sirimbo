import React from 'react';
import { AnnouncementForm } from '@app/ui/AnnouncementForm';
import { AnnouncementList } from '@app/ui/AnnouncementList';
import { Layout } from '@/components/layout/Layout';
import { WithSidebar } from '@app/ui/WithSidebar';
import { useRouter } from 'next/router';

const Page = () => {
  const router = useRouter();
  const onSuccess = React.useCallback((id: string) => router.push(`/nastenka/${id}`), [router]);
  return (
    <Layout requireMember>
      <WithSidebar sidebar={<AnnouncementList />}>
        <AnnouncementForm onSuccess={onSuccess} />
      </WithSidebar>
    </Layout>
  );
};

export default Page;
