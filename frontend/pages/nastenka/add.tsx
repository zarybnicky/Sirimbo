import React from 'react';
import { AnnouncementForm } from '@/ui/forms/AnnouncementForm';
import { AnnouncementList } from '@/ui/lists/AnnouncementList';
import { Layout } from '@/components/layout/Layout';
import { WithSidebar } from '@/ui/WithSidebar';
import { useRouter } from 'next/router';

export default function CreateAnnouncementPage() {
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
