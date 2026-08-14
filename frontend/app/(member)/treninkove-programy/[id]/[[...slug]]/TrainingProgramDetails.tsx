'use client';

import {
  CohortGroupDocument,
  type CohortGroupFragment,
} from '@/graphql/CohortGroup';
import { useActionMap } from '@/lib/actions';
import { cohortActions } from '@/lib/actions/cohort';
import { slugify } from '@/lib/slugify';
import { ActionGroup } from '@/ui/ActionGroup';
import { RichTextView } from '@/ui/RichTextView';
import { cardCls } from '@/ui/style';
import { PageHeader } from '@/ui/TitleBar';
import Link from 'next/link';
import { useQuery } from 'urql';

export function TrainingProgramDetails({
  initialItem,
}: {
  initialItem: CohortGroupFragment;
}) {
  const [{ data }] = useQuery({
    query: CohortGroupDocument,
    variables: { id: initialItem.id },
  });
  const item = data?.cohortGroup ?? initialItem;
  const cohortActionMap = useActionMap(cohortActions, item.cohortsList);

  return (
    <>
      <PageHeader title={item.name} />
      <div className="container py-4">
        <RichTextView className="mb-10" value={item.description} />
        {item.cohortsList.map((cohort) => (
          <div
            key={cohort.id}
            className={cardCls({ className: 'group break-inside-avoid pl-8' })}
          >
            <div className="mb-2 flex items-start justify-between gap-3">
              <h5 className="text-xl underline">
                <Link
                  href={`/treninkove-skupiny/${cohort.id}/${slugify(cohort.name)}`}
                >
                  {cohort.name}
                </Link>
              </h5>
              <ActionGroup actions={cohortActionMap.get(cohort.id)!} />
            </div>
            <h6 className="font-bold mb-2">{cohort.location}</h6>
            <RichTextView
              value={cohort.description
                .replaceAll('&nbsp;', ' ')
                .replaceAll('<br /> ', '')}
            />
            <div
              className="absolute rounded-l-lg w-4 border-r border-neutral-6 shadow-sm inset-y-0 left-0"
              style={{ backgroundColor: cohort.colorRgb }}
            />
          </div>
        ))}
      </div>
    </>
  );
}
