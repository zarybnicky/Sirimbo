import type {
  AnnouncementAudienceRole,
  UpsertAnnouncementInput,
} from '@/graphql';
import {
  AnnouncementAudienceFragment,
  type AnnouncementFragment,
  UpsertAnnouncementDocument,
} from '@/graphql/Announcement';
import { Checkbox, CheckboxElement } from '@/ui/fields/checkbox';
import { DatePickerElement } from '@/ui/fields/date';
import { RichTextEditor } from '@/ui/fields/richtext';
import { TextFieldElement } from '@/ui/fields/text';
import { RadioButtonGroupElement } from '@/ui/fields/RadioButtonGroupElement';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { AnnouncementAudienceBadges } from '@/ui/AnnouncementAudienceBadges';
import React from 'react';
import { type Control, useController, useForm, useWatch } from 'react-hook-form';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';
import { isTruthy } from '../../lib/truthyFilter';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { CohortListDocument } from '@/graphql/Cohorts';

const ROLE_OPTIONS: {
  value: AnnouncementAudienceRole;
  label: string;
}[] = [
  {
    value: 'MEMBER',
    label: 'Členové',
  },
  {
    value: 'TRAINER',
    label: 'Trenéři',
  },
  {
    value: 'ADMINISTRATOR',
    label: 'Administrátoři',
  },
];

const AUDIENCE_ROLE_VALUES = ['MEMBER', 'TRAINER', 'ADMINISTRATOR'] as const;
const STATUS_VALUES = ['DRAFT', 'PUBLISHED', 'ARCHIVED'] as const;
const STATUS_OPTIONS = [
  { id: 'DRAFT', label: 'Koncept' },
  { id: 'PUBLISHED', label: 'Veřejná' },
  { id: 'ARCHIVED', label: 'Archiv' },
];

const Form = z.object({
  title: z.string().min(1, 'Zadejte nadpis oznámení'),
  body: z.string().prefault(''),
  status: z.enum(STATUS_VALUES).prefault('PUBLISHED'),
  isSticky: z.boolean().prefault(false),
  scheduledSince: z.date().nullable().optional(),
  scheduledUntil: z.date().nullable().optional(),
  audienceRoles: z.array(z.enum(AUDIENCE_ROLE_VALUES)).prefault([]),
  cohortIds: z.array(z.string()).prefault([]),
});

export function AnnouncementForm({
  id,
  data,
  onSuccess,
}: {
  id?: string;
  data?: AnnouncementFragment | null;
  onSuccess?: (id: string | undefined) => void;
}) {
  const [result, upsert] = useMutation(UpsertAnnouncementDocument);

  const { reset, control, handleSubmit } = useForm({
    defaultValues: {
      status: 'DRAFT' as const,
      audienceRoles: [],
      cohortIds: [],
    },
    resolver: zodResolver(Form),
  });
  React.useEffect(() => {
    reset(
      {
        title: data?.title ?? '',
        body: data?.body ?? '',
        status: data?.status === 'SCHEDULED' ? 'PUBLISHED' : (data?.status ?? 'DRAFT'),
        isSticky: data?.isSticky ?? false,
        scheduledSince: data?.scheduledSince ? new Date(data.scheduledSince) : undefined,
        scheduledUntil: data?.scheduledUntil ? new Date(data.scheduledUntil) : undefined,
        audienceRoles:
          data?.announcementAudiences.nodes.map((x) => x.audienceRole).filter(isTruthy) ??
          [],
        cohortIds:
          data?.announcementAudiences.nodes.map((x) => x.cohortId).filter(isTruthy) ?? [],
      },
      {
        keepDirtyValues: true,
        keepTouched: true,
        keepErrors: true,
      },
    );
  }, [data, reset]);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    const oldAudiences = [...(data?.announcementAudiences.nodes || [])];
    const newAudiences: UpsertAnnouncementInput['audiences'] = [];

    for (const cohortId of values.cohortIds) {
      const existing = oldAudiences.findIndex((x) => x?.cohortId === cohortId);
      if (existing !== -1) {
        newAudiences.push({ cohortId, id: oldAudiences[existing]!.id });
        delete oldAudiences[existing];
      } else {
        newAudiences.push({ cohortId });
      }
    }
    for (const audienceRole of values.audienceRoles) {
      const existing = oldAudiences.findIndex((x) => x?.audienceRole === audienceRole);
      if (existing !== -1) {
        newAudiences.push({ audienceRole, id: oldAudiences[existing]!.id });
        delete oldAudiences[existing];
      } else {
        newAudiences.push({ audienceRole });
      }
    }
    // Remaining = unselected & to be deleted
    for (const remaining of oldAudiences) {
      newAudiences.push({ id: remaining.id });
    }

    const result = await upsert({
      input: {
        info: {
          id,
          title: values.title,
          body: values.body,
          status: values.status,
          isSticky: values.isSticky,
          scheduledSince: values.scheduledSince?.toISOString(),
          scheduledUntil: values.scheduledUntil?.toISOString(),
        },
        audiences: newAudiences,
      },
    });
    if (!result.error) {
      const newId = result.data?.upsertAnnouncement?.announcement?.id;
      if (!id && newId) toast.success('Přidáno.');
      onSuccess?.(newId);
    }
  };

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error} />

      <TextFieldElement control={control} name="title" label="Nadpis" required />
      <RichTextEditor
        initialState={data?.body}
        control={control}
        name="body"
        label="Text"
      />

      <div className="grid gap-2 sm:grid-cols-2">
        <RadioButtonGroupElement
          control={control}
          label="Stav"
          name="status"
          options={STATUS_OPTIONS}
        />
        <CheckboxElement
          control={control}
          name="isSticky"
          value="1"
          label="Připnout na stálou nástěnku"
        />
      </div>

      <div className="grid gap-2 sm:grid-cols-2">
        <DatePickerElement
          control={control}
          name="scheduledSince"
          label="Zveřejnit od"
          clearable
        />
        <DatePickerElement
          control={control}
          name="scheduledUntil"
          label="Archivovat od"
          clearable
        />
      </div>

      <AnnouncementAudienceEditor control={control} />

      <SubmitButton control={control} />
    </form>
  );
}

function AnnouncementAudienceEditor({
  control,
}: {
  control: Control<z.input<typeof Form>, unknown, z.infer<typeof Form>>;
}) {
  const { audienceRoles = [], cohortIds = [] } = useWatch({ control });

  const [{ data: cohortQuery, fetching: cohortsLoading }] = useQuery({
    query: CohortListDocument,
    variables: { archived: false },
  });

  const audiences: AnnouncementAudienceFragment[] = [
    ...audienceRoles.map((x) => ({
      id: '',
      cohortId: null,
      cohort: null,
      audienceRole: x,
    })),
    ...cohortIds.map((x) => ({
      id: '',
      cohortId: x,
      cohort: cohortQuery?.cohortsList?.find((c) => c.id === x) || null,
      audienceRole: null,
    })),
  ];

  const showWarning = audienceRoles.length === 0 && cohortIds.length === 0;

  return (
    <div className="space-y-2 rounded-md border border-neutral-6 bg-neutral-1 p-2">
      <h3 className="text-sm font-semibold text-neutral-12">Viditelnost</h3>

      <div>
        <AnnouncementAudienceBadges audiences={audiences} />
        {showWarning && (
          <div className="inline-flex items-center rounded-full border border-neutral-7 bg-neutral-2 px-2 py-0.5 text-[11px] font-medium uppercase tracking-wide text-neutral-11" >
            Viditelný pro všechny
          </div>
        )}
      </div>

      <div className="grid gap-2 sm:grid-cols-2">
        <section className="space-y-2">
          <h4 className="text-xs font-semibold uppercase tracking-wide text-neutral-10">
            Role
          </h4>
          <AudienceRoleCheckboxes control={control} />
        </section>

        <section className="space-y-2">
          <h4 className="text-xs font-semibold uppercase tracking-wide text-neutral-10">
            Skupiny
          </h4>
          <AudienceCohortCheckboxes
            control={control}
            cohorts={cohortQuery?.cohortsList}
            loading={cohortsLoading}
          />
        </section>
      </div>
    </div>
  );
}

function AudienceRoleCheckboxes({
  control,
}: {
  control: Control<z.input<typeof Form>, unknown, z.infer<typeof Form>>;
}) {
  const { field } = useController({ control, name: 'audienceRoles' });

  const toggle = React.useCallback(
    (role: AnnouncementAudienceRole) => {
      const next = new Set(field.value);
      if (next.has(role)) {
        next.delete(role);
      } else {
        next.add(role);
      }
      field.onChange([...next]);
    },
    [field],
  );

  return (
    <div className="space-y-1">
      {ROLE_OPTIONS.map((role) => (
        <Checkbox
          key={role.value}
          name={`announcement-role-${role.value}`}
          checked={field.value?.includes(role.value)}
          value={role.value}
          label={role.label}
          onChange={(event) => {
            event.stopPropagation();
            toggle(role.value);
          }}
        />
      ))}
    </div>
  );
}

function AudienceCohortCheckboxes({
  control,
  cohorts,
  loading,
}: {
  control: Control<z.input<typeof Form>, unknown, z.infer<typeof Form>>;
  cohorts:
    | { id: string; name?: string | null; colorRgb?: string | null }[]
    | null
    | undefined;
  loading?: boolean;
}) {
  const { field } = useController({ control, name: 'cohortIds' });

  const toggle = React.useCallback(
    (cohortId: string) => {
      const next = new Set(field.value);
      if (next.has(cohortId)) {
        next.delete(cohortId);
      } else {
        next.add(cohortId);
      }
      field.onChange([...next]);
    },
    [field],
  );

  if (loading) {
    return <div className="text-xs text-neutral-11">Načítám skupiny…</div>;
  }

  if (!cohorts || cohorts.length === 0) {
    return (
      <div className="text-xs text-neutral-11">Žádné skupiny nejsou k dispozici.</div>
    );
  }

  return (
    <div className="space-y-1">
      {cohorts.map((cohort) => {
        const checked = field.value?.includes(cohort.id);
        return (
          <Checkbox
            key={cohort.id}
            name={`announcement-cohort-${cohort.id}`}
            checked={checked}
            value={cohort.id}
            label={cohort.name ?? 'Bez názvu'}
            onChange={(event) => {
              event.stopPropagation();
              toggle(cohort.id);
            }}
          />
        );
      })}
    </div>
  );
}
