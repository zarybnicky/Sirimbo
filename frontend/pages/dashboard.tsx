import { useRouter } from 'next/router';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import * as React from 'react';
import { MyAnnouncements, StickyAnnouncements } from '@/ui/Announcements';
import { MyEventsList } from '@/ui/lists/MyEventsList';
import { CompetitionWeekPanel } from '@/ui/Competitions';
import { TabMenu } from '@/ui/TabMenu';
import { NextSeo } from 'next-seo';
import { Layout } from '@/ui/Layout';
import { parseAsString, useQueryState } from 'nuqs';

export default function DashboardPage() {
  const router = useRouter();
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const [variant, setVariant] = useQueryState(
    'tab',
    parseAsString.withDefault('myLessons').withOptions({ history: 'push' }),
  );

  const tabs = React.useMemo(
    () => [
      { id: 'myLessons', title: 'Moje události', contents: () => <MyEventsList /> },
      {
        id: 'competitions',
        title: 'Soutěže',
        contents: () => <CompetitionWeekPanel allowOnlyMine />,
      },
      { id: 'myAnnouncements', title: 'Aktuality', contents: () => <MyAnnouncements /> },
      {
        id: 'stickyAnnouncements',
        title: 'Stálá nástěnka',
        contents: () => <StickyAnnouncements />,
      },
    ],
    [],
  );

  if (!authLoading && auth.user && auth.personIds.length === 0) {
    void router.replace('/profil');
  }

  return (
    <Layout requireMember className="grow content relative content-stretch">
      <NextSeo title="Nástěnka" />
      <div className="col-full-width p-4 lg:py-8 h-full bg-neutral-2">
        <div className="xl:hidden">
          <TabMenu selected={variant} onSelect={setVariant} options={tabs} />
        </div>

        <div className="hidden xl:grid grid-cols-3 gap-4">
          <div className="flex flex-col gap-8">
            <MyEventsList />
            <CompetitionWeekPanel allowOnlyMine />
          </div>
          <MyAnnouncements />
          <StickyAnnouncements />
        </div>
      </div>
    </Layout>
  );
}
