import React from 'react';
import { PersonMembershipsDocument } from '@/graphql/Person';
import { PageHeader } from '@/ui/TitleBar';
import { useQuery } from 'urql';
import { useAuth } from '@/ui/use-auth';
import { formatAgeGroup } from '@/ui/format';
import { StringParam, useQueryParam } from 'use-query-params';
import { TabMenu } from '@/ui/TabMenu';
import { PersonMembershipView } from '@/ui/PersonMembershipView';
import { PersonAttendanceView } from '@/ui/PersonAttendanceView';
import { PersonPaymentsView } from '@/ui/PersonPaymentsView';
import { personActions } from '@/lib/actions/person';
import { useActions } from '@/lib/actions';

export function PersonView({ id }: { id: string }) {
  const auth = useAuth();
  const [{ data }] = useQuery({
    query: PersonMembershipsDocument,
    variables: { id },
    pause: !id,
  });
  const [tab, setTab] = useQueryParam('tab', StringParam);

  const isAdminOrCurrentPerson = auth.isAdmin || auth.isMyPerson(id);
  const item = data?.person;
  const actions = useActions(personActions, item);

  const tabs = React.useMemo(() => {
    if (!item) return [];

    const tabs = [
      {
        id: 'info',
        title: <>Členství</>,
        contents: () => <PersonMembershipView key="memberships" item={item} />,
      },
    ];
    if (isAdminOrCurrentPerson) {
      tabs.push(
        {
          id: 'events',
          title: <>Účasti</>,
          contents: () => <PersonAttendanceView id={id} />,
        },
        {
          id: 'payment',
          title: <>Platby</>,
          contents: () => <PersonPaymentsView key="payments" id={id} />,
        },
      );
    }
    return tabs;
  }, [id, item, isAdminOrCurrentPerson]);

  if (!item) return null;

  return (
    <>
      <PageHeader
        title={item.name}
        subtitle={[formatAgeGroup(item.birthDate), item.phone, item.email].join(' · ')}
        actions={actions}
      />

      <TabMenu selected={tab} onSelect={setTab} options={tabs} />
    </>
  );
}
