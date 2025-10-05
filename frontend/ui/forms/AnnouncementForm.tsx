import type { AnnouncementInput } from '@/graphql';
import {
  type AnnouncementFragment,
  CreateAnnouncementDocument,
  UpdateAnnouncementDocument,
} from '@/graphql/Announcement';
import { Checkbox, CheckboxElement } from '@/ui/fields/checkbox';
import { DatePickerElement } from '@/ui/fields/date';
import { RichTextEditor } from '@/ui/fields/richtext';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import {
  AnnouncementAudienceBadges,
  ANNOUNCEMENT_AUDIENCE_BADGES_MOCK,
  type AnnouncementAudienceRole,
} from '@/ui/AnnouncementAudienceBadges';
import { useCohorts } from '@/ui/useCohorts';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useController, useForm, type Control } from 'react-hook-form';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';

type FormProps = Pick<AnnouncementInput, 'title' | 'body' | 'isVisible' | 'isSticky'> & {
  scheduledSince: Date | undefined;
  scheduledUntil: Date | undefined;
  audienceRoles: AnnouncementAudienceRole[];
  cohortIds: string[];
};

const ROLE_OPTIONS: {
  value: AnnouncementAudienceRole;
  label: string;
  helperText: string;
}[] = [
  {
    value: 'member',
    label: 'Členové',
    helperText: 'Zobrazit oznámení přihlášeným členům.',
  },
  {
    value: 'trainer',
    label: 'Trenéři',
    helperText: 'Omezit viditelnost na trenéry klubu.',
  },
  {
    value: 'administrator',
    label: 'Administrátoři',
    helperText: 'Zobrazit pouze správcům systému.',
  },
];

type MaybeAnnouncementAudience = {
  announcementAudiencesByAnnouncementId?: {
    nodes?: (MaybeAnnouncementAudienceNode | null | undefined)[] | null;
  } | null;
  announcementAudiences?: {
    nodes?: (MaybeAnnouncementAudienceNode | null | undefined)[] | null;
  } | null;
  audienceRoles?: (AnnouncementAudienceRole | null | undefined)[] | null;
  audienceCohortIds?: (string | null | undefined)[] | null;
  cohortIds?: (string | null | undefined)[] | null;
};

type MaybeAnnouncementAudienceNode = {
  audienceRole?: AnnouncementAudienceRole | null;
  cohort?: MaybeAnnouncementCohort | null;
  cohortByUpsIdSkupina?: MaybeAnnouncementCohort | null;
  cohortByCohortId?: MaybeAnnouncementCohort | null;
};

type MaybeAnnouncementCohort = {
  id: string;
};

function extractAnnouncementAudience(
  data?: AnnouncementFragment | null,
): Pick<FormProps, 'audienceRoles' | 'cohortIds'> {
  const roles = new Set<AnnouncementAudienceRole>();
  const cohorts = new Set<string>();

  if (!data) {
    return { audienceRoles: [], cohortIds: [] };
  }

  const extended = data as unknown as MaybeAnnouncementAudience;

  extended.audienceRoles?.forEach((role) => {
    if (role) roles.add(role);
  });

  extended.audienceCohortIds?.forEach((cohortId) => {
    if (cohortId) cohorts.add(cohortId);
  });

  extended.cohortIds?.forEach((cohortId) => {
    if (cohortId) cohorts.add(cohortId);
  });

  const audienceConnections = [
    extended.announcementAudiencesByAnnouncementId,
    extended.announcementAudiences,
  ];

  audienceConnections.forEach((connection) => {
    connection?.nodes?.forEach((node) => {
      if (!node) return;
      if (node.audienceRole) {
        roles.add(node.audienceRole);
      }
      const cohort =
        node.cohort ?? node.cohortByUpsIdSkupina ?? node.cohortByCohortId;
      if (cohort?.id) {
        cohorts.add(cohort.id);
      }
    });
  });

  const legacy = data as unknown as {
    upozorneniSkupiniesByUpsIdRodic?: {
      nodes?: ({
        upsIdSkupina?: string | null;
        cohortByUpsIdSkupina?: { id: string } | null;
      } | null | undefined)[] | null;
    } | null;
  };

  legacy.upozorneniSkupiniesByUpsIdRodic?.nodes?.forEach((node) => {
    if (!node) return;
    if (node.upsIdSkupina) {
      cohorts.add(node.upsIdSkupina);
    }
    const cohort = node.cohortByUpsIdSkupina;
    if (cohort?.id) {
      cohorts.add(cohort.id);
    }
  });

  return {
    audienceRoles: Array.from(roles),
    cohortIds: Array.from(cohorts),
  };
}

export function AnnouncementForm({ id, data, onSuccess }: {
  id?: string;
  data?: AnnouncementFragment | null;
  onSuccess?: (id: string) => void;
}) {
  const create = useMutation(CreateAnnouncementDocument)[1];
  const update = useMutation(UpdateAnnouncementDocument)[1];

  const { reset, control, handleSubmit, watch } = useForm<FormProps>({
    defaultValues: {
      upNadpis: '',
      upText: '',
      isVisible: true,
      sticky: false,
      scheduledSince: undefined,
      scheduledUntil: undefined,
      audienceRoles: [],
      cohortIds: [],
    },
  });
  React.useEffect(() => {
    const audience = extractAnnouncementAudience(data);
    reset({
      title: data?.title,
      body: data?.body,
      isVisible: data ? data.isVisible : true,
      isSticky: data?.isSticky,
      scheduledSince: data?.scheduledSince ? new Date(data.scheduledSince) : undefined,
      scheduledUntil: data?.scheduledUntil ? new Date(data.scheduledUntil) : undefined,
      audienceRoles: audience.audienceRoles,
      cohortIds: audience.cohortIds,
    });
  }, [data, reset]);

  const audienceRoles = watch('audienceRoles');
  const cohortIds = watch('cohortIds');
  const { data: cohorts, fetching: cohortsLoading } = useCohorts();
  const availableCohorts = React.useMemo(() => {
    if (cohorts.length > 0) {
      return cohorts;
    }
    if (cohortsLoading) {
      return cohorts;
    }
    return ANNOUNCEMENT_AUDIENCE_BADGES_MOCK.cohorts ?? [];
  }, [cohorts, cohortsLoading]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    const patch = {
      title: values.title,
      body: values.body,
      isVisible: values.isVisible,
      isSticky: values.isSticky,
      scheduledSince: values.scheduledSince?.toISOString(),
      scheduledUntil: values.scheduledUntil?.toISOString(),
    };
    if (id) {
      await update({ id, patch });
      onSuccess?.(id);
    } else {
      const res = await create({ input: patch });
      const id = res.data?.createAnnouncement?.announcement?.id;
      if (id) {
        toast.success('Přidáno.');
        onSuccess?.(id);
      } else {
        reset();
      }
    }
  });

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />

      <TextFieldElement control={control} name="title" label="Nadpis" required />
      <RichTextEditor initialState={data?.body} control={control} name="body" label="Text" />
      <CheckboxElement control={control} name="isVisible" value="1" label="Viditelný" />
      <CheckboxElement control={control} name="isSticky" value="1" label="Připnout na stálou nástěnku" />
      <DatePickerElement control={control} name="scheduledSince" label="Odložit zveřejnění na den" />
      <DatePickerElement control={control} name="scheduledUntil" label="Skrýt příspěvek dne" />

      <AnnouncementAudienceEditor
        control={control}
        cohorts={availableCohorts}
        selectedRoles={audienceRoles}
        selectedCohortIds={cohortIds}
        loading={cohortsLoading}
      />

      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
}

function AnnouncementAudienceEditor({
  control,
  cohorts,
  selectedRoles,
  selectedCohortIds,
  loading,
}: {
  control: Control<FormProps>;
  cohorts: { id: string; name?: string | null; colorRgb?: string | null }[];
  selectedRoles?: AnnouncementAudienceRole[];
  selectedCohortIds?: string[];
  loading?: boolean;
}) {
  const roleList = selectedRoles ?? [];
  const cohortIdList = selectedCohortIds ?? [];

  const selectedCohorts = React.useMemo(() => {
    const map = new Map(cohorts.map((cohort) => [cohort.id, cohort]));
    return cohortIdList
      .map((id) => map.get(id))
      .filter((value): value is { id: string; name?: string | null; colorRgb?: string | null } => Boolean(value));
  }, [cohorts, cohortIdList]);

  const showWarning = roleList.length === 0 && cohortIdList.length === 0;

  return (
    <div className="space-y-4 rounded-md border border-neutral-6 bg-neutral-1 p-4">
      <div>
        <h3 className="text-sm font-semibold text-neutral-12">Cílové publikum</h3>
        <p className="text-xs text-neutral-11">
          Určete, které role nebo skupiny mají oznámení vidět.
        </p>
      </div>

      <section className="space-y-2">
        <h4 className="text-xs font-semibold uppercase tracking-wide text-neutral-10">Role</h4>
        <AudienceRoleCheckboxes control={control} />
      </section>

      <section className="space-y-2">
        <h4 className="text-xs font-semibold uppercase tracking-wide text-neutral-10">Skupiny</h4>
        <AudienceCohortCheckboxes control={control} cohorts={cohorts} loading={loading} />
      </section>

      <div className="space-y-2">
        <h4 className="text-xs font-semibold uppercase tracking-wide text-neutral-10">Shrnutí</h4>
        <AnnouncementAudienceBadges cohorts={selectedCohorts} roles={roleList} />
        {showWarning ? (
          <div className="rounded-md border border-amber-7 bg-amber-3 px-3 py-2 text-xs text-amber-12">
            Bez výběru konkrétního publika se oznámení zobrazí všem.
          </div>
        ) : null}
      </div>
    </div>
  );
}

function AudienceRoleCheckboxes({ control }: { control: Control<FormProps> }) {
  const { field } = useController({ control, name: 'audienceRoles' });
  const value: AnnouncementAudienceRole[] = field.value ?? [];

  const toggle = React.useCallback(
    (role: AnnouncementAudienceRole) => {
      const next = new Set(value);
      if (next.has(role)) {
        next.delete(role);
      } else {
        next.add(role);
      }
      field.onChange(Array.from(next));
    },
    [field, value],
  );

  return (
    <div className="space-y-1">
      {ROLE_OPTIONS.map((role) => {
        const checked = value.includes(role.value);
        return (
          <Checkbox
            key={role.value}
            name={`announcement-role-${role.value}`}
            checked={checked}
            value={role.value}
            label={role.label}
            helperText={role.helperText}
            onChange={(event) => {
              event.stopPropagation();
              toggle(role.value);
            }}
          />
        );
      })}
    </div>
  );
}

function AudienceCohortCheckboxes({
  control,
  cohorts,
  loading,
}: {
  control: Control<FormProps>;
  cohorts: { id: string; name?: string | null; colorRgb?: string | null }[];
  loading?: boolean;
}) {
  const { field } = useController({ control, name: 'cohortIds' });
  const value: string[] = field.value ?? [];

  const toggle = React.useCallback(
    (cohortId: string) => {
      const next = new Set(value);
      if (next.has(cohortId)) {
        next.delete(cohortId);
      } else {
        next.add(cohortId);
      }
      field.onChange(Array.from(next));
    },
    [field, value],
  );

  if (loading) {
    return <div className="text-xs text-neutral-11">Načítám skupiny…</div>;
  }

  if (!cohorts.length) {
    return <div className="text-xs text-neutral-11">Žádné skupiny nejsou k dispozici.</div>;
  }

  return (
    <div className="space-y-1">
      {cohorts.map((cohort) => {
        const checked = value.includes(cohort.id);
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
