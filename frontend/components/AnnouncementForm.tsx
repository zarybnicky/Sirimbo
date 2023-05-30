import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { UpozorneniInput } from 'lib/graphql';
import {
  AnnouncementDocument,
  CreateAnnouncementDocument,
  DeleteAnnouncementDocument,
  UpdateAnnouncementDocument,
} from 'lib/graphql/Announcement';
import { DatePickerElement } from './DateRange';
import dynamic from 'next/dynamic';
import { useMutation, useQuery } from 'urql';
import { CheckboxElement } from './Checkbox';
import { DeleteButton } from './DeleteButton';
import { Item } from './layout/Item';
import { toast } from 'react-toastify';
import { useRouter } from 'next/router';
import { Route } from 'nextjs-routes';
import { ErrorPage } from './ErrorPage';
const RichTextEditor = dynamic(() => import('./RichTextEditor'), { ssr: false });

type FormProps = Pick<
  UpozorneniInput,
  'upNadpis' | 'upText' | 'isVisible'
> & {
  scheduledSince: Date | undefined;
  scheduledUntil: Date | undefined;
};

const backHref: Route = { pathname: '/admin/nastenka' };

export const AnnouncementForm = ({ id = '' }: {id?: string}) => {
  const router = useRouter();
  const [query] = useQuery({query: AnnouncementDocument, variables: { id }});
  const data = query.data?.upozorneni;

  const create = useMutation(CreateAnnouncementDocument)[1];
  const update = useMutation(UpdateAnnouncementDocument)[1];

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      upNadpis: data?.upNadpis,
      upText: data?.upText,
      isVisible: data?.isVisible,
      scheduledSince: data?.scheduledSince ? new Date(data.scheduledSince) : undefined,
      scheduledUntil: data?.scheduledUntil ? new Date(data.scheduledUntil) : undefined,
    });
  }, [data, reset]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    const patch = {
      upNadpis: values.upNadpis,
      upText: values.upText,
      isVisible: values.isVisible,
      scheduledSince: values.scheduledSince?.toISOString(),
      scheduledUntil: values.scheduledUntil?.toISOString(),
    };
    if (id) {
      await update({ id, patch });
    } else {
      const res = await create({ input: patch });
      const id = res.data?.createUpozorneni?.upozorneni?.id;
      toast.success('Přidáno.');
      if (id) {
        router.replace({ pathname: '/admin/nastenka/[id]', query: { id } });
      } else {
        reset(undefined);
      }
    }
  });

  if (query.data && query.data.upozorneni === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form
      className="container flex flex-col gap-2"
      onSubmit={handleSubmit(onSubmit.execute)}
    >
      <Item.Titlebar
        backHref={backHref}
        title={id ? data?.upNadpis || '(Bez názvu)' : 'Nový příspěvek'}
      >
        {id && (
          <DeleteButton
            doc={DeleteAnnouncementDocument}
            id={id}
            title="smazat příspěvek"
            onDelete={() => router.push(backHref)}
          />
        )}
        <SubmitButton loading={onSubmit.loading} />
      </Item.Titlebar>

      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="upNadpis" label="Nadpis" required />
      <DatePickerElement
        control={control}
        name="scheduledSince"
        label="Odložit zveřejnění na den"
      />
      <DatePickerElement
        control={control}
        name="scheduledUntil"
        label="Skrýt příspěvek dne"
      />
      <CheckboxElement control={control} name="isVisible" value="1" label="Viditelný" />
      <RichTextEditor
        initialState={data?.upText}
        control={control}
        name="upText"
        label="Text"
      />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
