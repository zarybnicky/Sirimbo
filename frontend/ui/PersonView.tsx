import React from 'react';
import { PersonMembershipsDocument } from '@/graphql/Person';
import { PageHeader } from '@/ui/TitleBar';
import { formatCstsClass, getBestCstsProgress } from '@/ui/csts';
import { useQuery } from 'urql';
import { useAuth } from '@/ui/use-auth';
import { formatAgeGroup } from '@/ui/format';
import { StringParam, useQueryParam } from 'use-query-params';
import { TabMenu } from '@/ui/TabMenu';
import { PersonMembershipView } from '@/ui/PersonMembershipView';
import { PersonAttendanceView } from '@/ui/PersonAttendanceView';
import { PersonPaymentsView } from '@/ui/PersonPaymentsView';
import { PersonWorkReportView } from '@/ui/PersonWorkReportView';
import { personActions } from '@/lib/actions/person';
import { useActions } from '@/lib/actions';
import { tenantIdAtom } from '@/ui/state/auth';
import { useAtomValue } from 'jotai';

export function PersonView({ id }: { id: string }) {
  const auth = useAuth();
  const tenantId = useAtomValue(tenantIdAtom);
  const [{ data }] = useQuery({
    query: PersonMembershipsDocument,
    variables: { id },
    pause: !id,
  });
  const [tab, setTab] = useQueryParam('tab', StringParam);

  const isAdminOrCurrentPerson = auth.isAdmin || auth.isMyPerson(id);
  const item = data?.person;
  const isCurrentTenantTrainer = item?.tenantTrainersList.some(
    (trainer) => trainer.tenantId === tenantId && trainer.status === 'ACTIVE',
  );
  const actions = useActions(personActions, item);
  const sttProgress = getBestCstsProgress(item?.cstsProgressList, 'Standard');
  const latProgress = getBestCstsProgress(item?.cstsProgressList, 'Latin');
  const sttClass = formatCstsClass(sttProgress?.category?.class);
  const latClass = formatCstsClass(latProgress?.category?.class);
  const categoryProgress = [
    formatAgeGroup(item?.birthDate),
    sttClass || latClass ? `${sttClass ?? '-'}/${latClass ?? '-'}` : null,
  ]
    .filter(Boolean)
    .join(' ');

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
    if (isAdminOrCurrentPerson && isCurrentTenantTrainer) {
      tabs.push({
        id: 'workReport',
        title: <>Výkaz práce</>,
        contents: () => <PersonWorkReportView key="work-report" id={id} />,
      });
    }
    return tabs;
  }, [id, item, isAdminOrCurrentPerson, isCurrentTenantTrainer]);

  if (!item) return null;

  return (
    <>
      <PageHeader
        title={item.name}
        subtitle={[categoryProgress, item.phone, item.email].filter(Boolean).join(' · ')}
        actions={actions}
      />

      <TabMenu selected={tab} onSelect={setTab} options={tabs} />
    </>
  );
}
