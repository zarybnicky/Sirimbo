import * as React from 'react';
import { saveAs } from 'file-saver';
import { EventDocument } from '@app/graphql/Event';
import { useQuery } from 'urql';
import { buttonCls } from '@app/ui/style';
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
        { header: 'Pár/jednotlivec', key: 'person' },
        { header: 'Datum přihlášení', key: 'registered' },
        { header: 'Poznámka', key: 'note' },
      ];
      data.event?.eventTrainersList.forEach(trainer => {
        columns.push({
          header: trainer.person?.name || '?',
          key: trainer.id,
        })
      });
      worksheet.columns = columns;

      worksheet.getRow(1).font = { bold: true };
      worksheet.columns.forEach((column) => {
        column.width = (column?.header?.length || 0) + 10;
        column.alignment = { horizontal: 'center' };
      });

      data.event?.eventRegistrationsList?.forEach((x) => {
        const row: { [key: string]: string } = {
          person: x.person?.name || `${x.couple?.man?.name} - ${x.couple?.woman?.name}`,
          registered: fullDateFormatter.format(new Date(x.createdAt)),
          note: x.note || '',
        };
        x.eventLessonDemandsByRegistrationIdList.forEach(demand => {
          row[demand.trainerId] = demand.lessonCount.toString();
        });
        console.log(row);
        worksheet.addRow(row);
      });
      console.log(worksheet.columns);

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
