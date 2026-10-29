'use client';

import { PageHeader } from '@/ui/PageHeader';
import { parseAsString, useQueryState } from 'nuqs';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { PaymentList } from './PaymentList';
import { TransactionList } from './TransactionList';
import { AccountList } from './AccountList';

export function Payments() {
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );

  return (
    <>
      <PageHeader title="Platby" />
      <TabMenu selected={tab} onSelect={setTab}>
        <Tab id="info" title="Stav kreditu">
          <AccountList />
        </Tab>
        <Tab id="payments" title="Platby">
          <PaymentList />
        </Tab>
        <Tab id="transactions" title="Transakce">
          <TransactionList />
        </Tab>
      </TabMenu>
    </>
  );
}
