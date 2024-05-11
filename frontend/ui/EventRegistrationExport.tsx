import * as React from 'react';
import { saveAs } from 'file-saver';
import { EventDocument } from '@/graphql/Event';
import { useQuery } from 'urql';
import { buttonCls } from '@/ui/style';
import { fullDateFormatter } from './format';

export function EventRegistrationExport({ id }: { id: string }) {
  const [{ data }] = useQuery({query: EventDocument, variables: { id }, pause: !id});

  const saveData = React.useCallback(
    async (e?: React.MouseEvent) => {
      e?.preventDefault();
      if (!data) {
        return;
      }
      const { Workbook } = await import('exceljs');
      const workbook = new Workbook();
      const worksheet = workbook.addWorksheet(data.event?.name || 'Sheet 1');

      const columns = [
        { header: 'Partner', key: 'man' },
        { header: 'Partnerka', key: 'woman' },
        { header: 'Datum přihlášení', key: 'registered' },
        { header: 'Poznámka', key: 'note' },
      ];
      data.event?.eventTrainersList.forEach(trainer => {
        columns.push({
          header: trainer.name || '?',
          key: trainer.id,
        })
      });
      worksheet.columns = columns;

      worksheet.getRow(1).font = { bold: true };
      worksheet.columns.forEach((column) => {
        column.width = (column?.header?.length || 0) + 10;
        column.alignment = { horizontal: 'center' };
      });

      const rows: { [k: string]: string }[] = [];
      data.event?.eventRegistrationsList?.forEach((x) => {
        const row: { [key: string]: string } = {
          man: x.person?.name || x.couple?.man?.name || '',
          woman: x.couple?.woman?.name || '',
          registered: fullDateFormatter.format(new Date(x.createdAt)),
          note: x.note || '',
        };
        x.eventLessonDemandsByRegistrationIdList.forEach(demand => {
          row[demand.trainerId] = demand.lessonCount.toString();
        });
        rows.push(row);
      });
      rows.sort((a, b) => (a.man as string).localeCompare(b.man as string))
      rows.forEach(x => worksheet.addRow(x));

      const buf = await workbook.xlsx.writeBuffer();
      saveAs(new Blob([buf]), `${data.event?.name || 'export-akce'}.xlsx`);
    },
    [data],
  );

  return (
    <button type="button" className={buttonCls({ variant: 'outline' })} onClick={saveData}>
      Export přihlášek
    </button>
  );
}
