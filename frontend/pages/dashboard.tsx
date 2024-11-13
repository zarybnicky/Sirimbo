import { useRouter } from 'next/router';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import * as React from 'react';
import { MyAnnouncements } from '@/ui/MyAnnouncements';
import { MyEventsList } from '@/ui/lists/MyEventsList';
import { TabMenu } from '@/ui/TabMenu';
import { StickyAnnouncements } from '@/ui/StickyAnnouncements';
import { NextSeo } from 'next-seo';
import { Layout } from '@/components/layout/Layout';
import { useQueryParam, withDefault, StringParam } from 'use-query-params';

export default function DashboardPage() {
  const router = useRouter();
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const [variant, setVariant] = useQueryParam('tab', withDefault(StringParam, 'myLessons'));

  if (!authLoading && auth.user && !auth.personIds.length) {
    void router.replace('/profil');
  }

  return (
    <Layout requireMember className="grow content relative content-stretch">
      <NextSeo title="Nástěnka" />
      <div className="col-full-width p-4 lg:py-8 h-full bg-neutral-2">
        <div className="xl:hidden">
          <TabMenu
            selected={variant}
            onSelect={setVariant}
            options={[
              { id: 'myLessons', label: 'Moje události' },
              { id: 'myAnnouncements', label: 'Aktuality' },
              { id: 'stickyAnnouncements', label: 'Stálá nástěnka' },
            ]}
          />
          <div className="mt-4">
            {variant === 'myLessons' ? (
              <MyEventsList />
            ) : variant === 'myAnnouncements' ? (
              <MyAnnouncements />
            ) : (
              <StickyAnnouncements />
            )}
          </div>
        </div>

        <div className="hidden xl:grid grid-cols-3 gap-4">
          <MyEventsList />
          <MyAnnouncements />
          <StickyAnnouncements />
        </div>
      </div>
    </Layout>
  );
}
