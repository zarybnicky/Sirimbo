'use client';

import { MyAnnouncements, StickyAnnouncements } from '@/ui/Announcements';
import { CompetitionWeekPanel } from '@/ui/Competitions';
import { MyEventsList } from '@/ui/lists/MyEventsList';
import { TabMenu } from '@/ui/TabMenu';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import { redirect } from 'next/navigation';
import { parseAsString, useQueryState } from 'nuqs';

const tabs = [
  { id: 'myLessons', title: 'Moje události', contents: MyEventsList },
  {
    id: 'competitions',
    title: 'Soutěže',
    contents: () => <CompetitionWeekPanel allowOnlyMine />,
  },
  { id: 'myAnnouncements', title: 'Aktuality', contents: MyAnnouncements },
  {
    id: 'stickyAnnouncements',
    title: 'Stálá nástěnka',
    contents: StickyAnnouncements,
  },
];

export function Dashboard() {
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const [variant, setVariant] = useQueryState(
    'tab',
    parseAsString.withDefault('myLessons').withOptions({ history: 'push' }),
  );

  if (authLoading || !auth.user) return null;
  if (auth.personIds.length === 0) redirect('/profil');

  return (
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
  );
}
