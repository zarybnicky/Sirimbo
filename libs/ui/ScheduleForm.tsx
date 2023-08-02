import {
  ScheduleDocument,
  ScheduleItemBasicFragment,
} from '@app/graphql/Schedule';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from '@app/ui/fields/text';
import { CheckboxElement } from '@app/ui/fields/checkbox';
import { TrainerListDocument } from '@app/graphql/User';
import { ComboboxElement } from './Combobox';
import { ErrorPage } from './ErrorPage';
import { formatCoupleName } from '@app/ui/format-name';
import { X } from 'lucide-react';
import { useQuery } from 'urql';
import { TitleBar } from '@app/ui/TitleBar';
import { Card } from '@app/ui/Card';
import { CoupleListDocument } from '@app/graphql/Couple';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { AdminEntity } from './generic/AdminEntityList';

export const ScheduleForm = ({ entity, id = '' }: { entity: AdminEntity; id?: string }) => {
  const [query] = useQuery({ query: ScheduleDocument, variables: { id } });
  const data = query.data?.rozpi;
  const title = id ? data?.userByRTrener?.fullName || '(Bez názvu)' : 'Nový rozpis';

  const [{ data: trainers }] = useQuery({ query: TrainerListDocument });

  const { reset, control } = useForm();
  React.useEffect(() => {
    reset({
      rKde: data?.rKde,
      rDatum: data?.rDatum,
      rTrener: data?.rTrener,
      rVisible: data?.rVisible,
      rLock: data?.rLock,
    });
  }, [data, reset]);

  if (query.data && query.data.rozpi === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="container space-y-2">
      <TitleBar backHref={entity.listRoute} title={title}/>

      <ComboboxElement
        control={control}
        name="rTrener"
        label="Trenér"
        placeholder="vyberte trenéra"
        options={(trainers?.trainers?.nodes || []).map((x) => ({
          id: x.id,
          label: `${x.uJmeno} ${x.uPrijmeni}`,
        }))}
      />
      <TextFieldElement control={control} name="rKde" label="Místo" required />
      <TextFieldElement
        control={control}
        type="date"
        label="Datum"
        name="rDatum"
        required
      />
      <CheckboxElement control={control} name="rVisible" value="1" label="Viditelný" />
      <CheckboxElement control={control} name="rLock" value="1" label="Uzamčený" />

      {id && (
        <div className="mt-1 pb-8 bg-neutral-0 p-3 rounded-md border border-accent-9 space-y-2">
          {data?.rozpisItemsByRiIdRodic.nodes.map((x) => (
            <LessonAdminForm key={x.id} lesson={x} />
          ))}
          {/* <LessonAddForm /> */}
        </div>
      )}
    </form>
  );
};

const LessonForm = z.object({
  riOd: z.string().regex(/[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?/),
  riDo: z.string().regex(/[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?/),
  riPartner: z.string().nullish(),
  riLock: z.boolean().optional(),
}).refine((form) => form.riOd < form.riDo, 'Čas začátku musí být před časem konce')
type LessonFormProps = z.infer<typeof LessonForm>;

function LessonAdminForm({ lesson }: { lesson: ScheduleItemBasicFragment }) {
  const [{ data: couples }] = useQuery({ query: CoupleListDocument });
  const [mode, setMode] = React.useState<'view' | 'edit'>('view');
  const { reset, control } = useForm<LessonFormProps>({
    resolver: zodResolver(LessonForm),
  });
  const couple = lesson.paryByRiPartner;

  React.useEffect(() => {
    reset(LessonForm.innerType().partial().optional().parse(lesson));
  }, [mode, reset, lesson]);

  return mode === 'view' ? (
    <Card
      className="flex gap-2 tabular-nums cursor-pointer"
      onClick={() => setMode('edit')}
    >
      <div>
        {lesson.riOd.substring(0, 5)} - {lesson.riDo.substring(0, 5)}
      </div>
      <div className="grow">{couple ? formatCoupleName(couple) : 'VOLNÁ'}</div>
    </Card>
  ) : (
    <Card>
        <button
          type="button"
          onClick={() => setMode('view')}
        >
          <X />
        </button>

        <TextFieldElement control={control} name="riOd" type="text" />
        <TextFieldElement control={control} name="riDo" type="text" />
        <ComboboxElement
          control={control}
          name="riPartner"
          placeholder="vyberte pár"
          options={(couples?.activeCouples?.nodes || []).map(x => ({ id: x.id, label: formatCoupleName(x) }))}
        />
        <CheckboxElement control={control} name="riLock" />
    </Card>
  );
}
