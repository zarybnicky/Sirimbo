'use client';

import {
  CohortWithMembersDocument,
  type CohortWithMembersQuery,
  type CstsProgressRecordFragment,
} from '@/graphql/Cohorts';
import { useQuery } from 'urql';
import { cohortActions } from '@/lib/actions/cohort';
import { cohortMembershipActions } from '@/lib/actions/cohortMembership';
import { useActionMap, useActions } from '@/lib/actions';
import { DocumentPane, useCreateDocument } from '@/ui/DocumentPane';
import { RichTextView } from '@/ui/RichTextView';
import { PageHeader } from '@/ui/TitleBar';
import { formatCstsClass, getBestCstsProgress } from '@/ui/csts';
import { formatAgeGroup, formatOpenDateRange } from '@/ui/format';
import { typographyCls } from '@/ui/style';
import Link from 'next/link';
import React from 'react';
import { ActionRow } from '@/ui/ActionRow';
import { personActions } from '@/lib/actions/person';
import { isTruthy } from '@/lib/truthyFilter';
import { ActivityTimeline } from '@/ui/ActivityTimeline';
import { useAuth } from '@/lib/auth';

export function TrainingGroup({
  initialCohort,
}: {
  initialCohort: NonNullable<CohortWithMembersQuery['entity']>;
}) {
  const auth = useAuth();
  const [{ data }, refetch] = useQuery({
    query: CohortWithMembersDocument,
    variables: { id: initialCohort.id },
  });
  const cohort = data?.entity ?? initialCohort;

  const members = React.useMemo(
    () => cohort.cohortMembershipsList ?? [],
    [cohort.cohortMembershipsList],
  );
  const membershipActionMap = useActionMap(cohortMembershipActions, members);
  const memberActionMap = useActionMap(
    personActions,
    members.map((x) => x.person).filter(isTruthy),
  );
  const description = React.useMemo(
    () => cohort.description?.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', ''),
    [cohort.description],
  );
  const actions = useActions(cohortActions, cohort);
  const subject = React.useMemo(
    () => ({ cohortId: initialCohort.id }),
    [initialCohort.id],
  );
  const [creatingPlan, createDocument] = useCreateDocument(subject);
  const addPlan = React.useCallback(async () => {
    if (await createDocument('Nový plán')) {
      refetch({ requestPolicy: 'network-only' });
    }
  }, [createDocument, refetch]);

  return (
    <>
      <PageHeader title={cohort.name} actions={actions} />

      <h6 className="mb-2 font-bold">{cohort.location}</h6>
      <RichTextView value={description} />

      {(auth.isTrainerOrAdmin || (cohort.documentsList ?? []).length > 0) && (
        <section className="my-4">
          <div className="flex items-baseline justify-between">
            <h3 className={typographyCls({ variant: 'section', className: 'my-3' })}>
              Plány
            </h3>
            {auth.isTrainerOrAdmin && (
              <button
                type="button"
                onClick={addPlan}
                disabled={creatingPlan}
                className="text-sm text-accent-11 hover:underline disabled:opacity-50"
              >
                + Plán
              </button>
            )}
          </div>
          {(cohort.documentsList ?? []).map((doc) => (
            <article key={doc.id} className="mb-4">
              <h4 className="mb-1 font-bold">{doc.title || 'Plán'}</h4>
              <DocumentPane id={doc.id} />
            </article>
          ))}
        </section>
      )}

      {auth.isLoggedIn && (
        <>
          <h3 className={typographyCls({ variant: 'section', className: 'my-3' })}>
            Členové ({members.length})
          </h3>

          <div className="grid grid-cols-2 gap-x-4 gap-y-3 lg:grid-cols-[1fr_minmax(0,14rem)_minmax(0,14rem)_auto] pb-4">
            {members.map((membership) => (
              <div
                key={membership.id}
                className="col-span-full grid grid-cols-subgrid items-center gap-x-4 gap-y-2 text-sm"
              >
                <ActionRow
                  className="mb-0 min-w-max"
                  actions={[
                    ...(membership.person
                      ? memberActionMap.get(membership.person.id)!
                      : []),
                    ...membershipActionMap.get(membership.id)!,
                  ]}
                >
                  {membership.person ? (
                    <span className="inline-flex items-center gap-1">
                      <Link
                        className="font-bold underline"
                        href={`/clenove/${membership.person.id}`}
                      >
                        {membership.person.name}
                      </Link>
                    </span>
                  ) : (
                    '?'
                  )}
                </ActionRow>

                <div className="order-3 lg:order-2">
                  <CategoryList person={membership.person} discipline="Standard" />
                </div>
                <div className="order-4 lg:order-3">
                  <CategoryList person={membership.person} discipline="Latin" />
                </div>

                <div className="order-2 text-right lg:order-4">
                  {formatOpenDateRange(membership)}
                </div>
              </div>
            ))}
          </div>

          <ActivityTimeline cohortId={cohort.id} />
        </>
      )}
    </>
  );
}

function CategoryList({
  person,
  discipline,
}: {
  person?: {
    cstsId?: number | null;
    birthDate?: string | null;
    cstsProgressList?: CstsProgressRecordFragment[] | null;
  } | null;
  discipline: string;
}) {
  if (!person?.cstsId)
    return (
      <div className="flex justify-center">
        <div className="px-2 border-2 border-neutral-8 rounded-full text-neutral-9 text-xs">
          ? chybí IDT
        </div>
      </div>
    );
  if (!person?.cstsProgressList) return null;
  const item = getBestCstsProgress(person.cstsProgressList, discipline);
  if (!item) return null;

  return (
    <div className="flex flex-col leading-tight text-center">
      <div className="font-medium">
        {[
          formatAgeGroup(person.birthDate),
          formatCstsClass(item.category?.class),
          item.category?.discipline === 'Standard_Latin'
            ? 'STT+LAT'
            : item.category?.discipline === 'Standard'
              ? 'STT'
              : item.category?.discipline === 'Latin'
                ? 'LAT'
                : item.category?.discipline,
        ]
          .filter(Boolean)
          .join(' ')}
      </div>
      <div className="text-xs text-neutral-11">
        {Number.parseFloat(item.points ?? '0')} / {item.finals}F
      </div>
    </div>
  );
}
