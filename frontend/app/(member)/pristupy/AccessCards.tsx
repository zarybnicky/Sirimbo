'use client';

import { AccessEventList } from '@/ui/AccessEventList';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { PageHeader } from '@/ui/PageHeader';
import { PeopleAccessOverview } from '@/ui/PeopleAccessOverview';
import { parseAsString, useQueryState } from 'nuqs';

export function AccessCards() {
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );

  return (
    <>
      <PageHeader title="Přístupy" />
      <TabMenu selected={tab} onSelect={setTab}>
        <Tab id="people" title="Osoby">
          <PeopleAccessOverview />
        </Tab>
        <Tab id="events" title="Přístupy" requireStarletImport>
          <AccessEventList />
        </Tab>
      </TabMenu>
    </>
  );
}
