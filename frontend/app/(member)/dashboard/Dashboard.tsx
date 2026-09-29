'use client';

import { MyAnnouncements, StickyAnnouncements } from '@/ui/Announcements';
import { CompetitionWeekPanel } from '@/ui/Competitions';
import { MyEventsList } from '@/ui/lists/MyEventsList';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { useAuth, useAuthLoading } from '@/lib/auth';
import { parseAsString, useQueryState } from 'nuqs';

export function Dashboard() {
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const [variant, setVariant] = useQueryState(
    'tab',
    parseAsString.withDefault('myLessons').withOptions({ history: 'push' }),
  );

  if (authLoading || !auth.userId) return null;

  return (
    <div className="col-full-width p-4 lg:py-8 h-full bg-neutral-2">
      <div className="xl:hidden">
        <TabMenu selected={variant} onSelect={setVariant}>
          <Tab id="myLessons" title="Moje události">
            <MyEventsList />
          </Tab>
          <Tab id="competitions" title="Soutěže">
            <CompetitionWeekPanel allowOnlyMine />
          </Tab>
          <Tab id="myAnnouncements" title="Aktuality">
            <MyAnnouncements />
          </Tab>
          <Tab id="stickyAnnouncements" title="Stálá nástěnka">
            <StickyAnnouncements />
          </Tab>
        </TabMenu>
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
