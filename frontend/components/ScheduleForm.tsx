import {
  CreateScheduleDocument,
  DeleteScheduleDocument,
  ScheduleDocument,
  DeleteLessonDocument,
  ScheduleItemBasicFragment,
  UpdateScheduleDocument,
  UpdateLessonDocument,
} from '@app/graphql/Schedule';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { RozpiInput } from '@app/graphql';
import { TrainerListDocument } from '@app/graphql/User';
import { useRouter } from 'next/router';
import { ComboboxElement } from './Combobox';
import { Route } from 'nextjs-routes';
import { toast } from 'react-toastify';
import { ErrorPage } from './ErrorPage';
import { DeleteButton } from './DeleteButton';
import { formatCoupleName } from 'lib/format-name';
import { X, Check } from 'lucide-react';
import { useMutation, useQuery } from 'urql';
import { TitleBar } from './layout/TitleBar';
import { Card } from './Card';
import { CoupleListDocument } from '@app/graphql/Couple';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';

type FormProps = Pick<RozpiInput, 'rTrener' | 'rKde' | 'rDatum' | 'rVisible' | 'rLock'>;

const backHref: Route = { pathname: '/admin/akce' };

export const ScheduleForm = ({ id = '' }: { id?: string }) => {
  const router = useRouter();
  const [query] = useQuery({ query: ScheduleDocument, variables: { id } });
  const data = query.data?.rozpi;
  const title = id ? data?.userByRTrener?.fullName || '(Bez názvu)' : 'Nový rozpis';

  const create = useMutation(CreateScheduleDocument)[1];
  const update = useMutation(UpdateScheduleDocument)[1];

  const [{ data: trainers }] = useQuery({ query: TrainerListDocument });

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      rKde: data?.rKde,
      rDatum: data?.rDatum,
      rTrener: data?.rTrener,
      rVisible: data?.rVisible,
      rLock: data?.rLock,
    });
  }, [data, reset]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await update({ id, patch: values });
    } else {
      const res = await create({ input: values });
      const id = res.data?.createRozpi?.rozpi?.id;
      toast.success('Přidáno.');
      if (id) {
        router.replace({ pathname: '/admin/rozpis/[id]', query: { id } });
      } else {
        reset(undefined);
      }
    }
  });

  if (query.data && query.data.rozpi === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="container space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TitleBar backHref={backHref} title={title}>
        {id && (
          <DeleteButton
            doc={DeleteScheduleDocument}
            id={id}
            title="smazat rozpis"
            onDelete={() => router.push(backHref)}
          />
        )}
        <SubmitButton loading={onSubmit.loading} />
      </TitleBar>

      <ErrorBox error={onSubmit.error} />
      <ComboboxElement
        control={control}
        name="rTrener"
        label="Trenér"
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
        <div className="mt-1 pb-8 bg-white p-3 rounded-md border border-red-500 space-y-2">
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
  riPartner: z.string(),
  riLock: z.boolean().optional(),
}).refine((form) => form.riOd < form.riDo, 'Čas začátku musí být před časem konce')
type LessonFormProps = z.infer<typeof LessonForm>;

function LessonAdminForm({ lesson }: { lesson: ScheduleItemBasicFragment }) {
  const [{ data: couples }] = useQuery({ query: CoupleListDocument });
  const update = useMutation(UpdateLessonDocument)[1];
  const [mode, setMode] = React.useState<'view' | 'edit'>('view');
  const { reset, control, handleSubmit } = useForm<LessonFormProps>({
    resolver: zodResolver(LessonForm),
  });
  const couple = lesson.paryByRiPartner;

  React.useEffect(() => {
    reset(LessonForm.optional().parse(lesson));
  }, [mode, reset, lesson]);

  const onSubmit = React.useCallback((patch: LessonFormProps) => {
    setMode('view');
    update({ id: lesson.id, patch})
  }, [update, lesson.id]);

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
      <form onSubmit={handleSubmit(onSubmit)}>
        <button
          type="button"
          onClick={() => setMode('view')}
        >
          <X />
        </button>
        <button type="submit">
          <Check />
        </button>

        <TextFieldElement control={control} name="riOd" type="text" />
        <TextFieldElement control={control} name="riDo" type="text" />
        <ComboboxElement control={control} name="riPartner" options={(couples?.activeCouples?.nodes || []).map(x => ({ id: x.id, label: formatCoupleName(x) }))} />
        <CheckboxElement control={control} name="riLock" />
        <DeleteButton doc={DeleteLessonDocument} id={lesson.id} title="smazat lekci" />
      </form>
    </Card>
  );
}
