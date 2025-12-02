const { Document, Packer, Paragraph, TextRun, Table, TableRow, TableCell, 
        AlignmentType, LevelFormat, BorderStyle, WidthType, ShadingType,
        HeadingLevel, PageNumber, Header, Footer, PageBreak } = require('docx');
const fs = require('fs');

// Table border styling
const tableBorder = { style: BorderStyle.SINGLE, size: 1, color: "CCCCCC" };
const cellBorders = { top: tableBorder, bottom: tableBorder, left: tableBorder, right: tableBorder };

// Create the document
const doc = new Document({
  styles: {
    default: { document: { run: { font: "Arial", size: 22 } } }, // 11pt default
    paragraphStyles: [
      { id: "Title", name: "Title", basedOn: "Normal",
        run: { size: 52, bold: true, color: "1a365d", font: "Arial" },
        paragraph: { spacing: { before: 240, after: 240 }, alignment: AlignmentType.CENTER } },
      { id: "Heading1", name: "Heading 1", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { size: 32, bold: true, color: "2c5282", font: "Arial" },
        paragraph: { spacing: { before: 360, after: 120 }, outlineLevel: 0 } },
      { id: "Heading2", name: "Heading 2", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { size: 26, bold: true, color: "2b6cb0", font: "Arial" },
        paragraph: { spacing: { before: 240, after: 80 }, outlineLevel: 1 } },
      { id: "Heading3", name: "Heading 3", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { size: 24, bold: true, color: "3182ce", font: "Arial" },
        paragraph: { spacing: { before: 200, after: 60 }, outlineLevel: 2 } },
      { id: "Code", name: "Code Block", basedOn: "Normal",
        run: { size: 18, font: "Courier New", color: "2d3748" },
        paragraph: { spacing: { before: 60, after: 60 } } }
    ]
  },
  numbering: {
    config: [
      { reference: "main-bullets",
        levels: [{ level: 0, format: LevelFormat.BULLET, text: "•", alignment: AlignmentType.LEFT,
          style: { paragraph: { indent: { left: 720, hanging: 360 } } } }] },
      { reference: "sub-bullets",
        levels: [{ level: 0, format: LevelFormat.BULLET, text: "○", alignment: AlignmentType.LEFT,
          style: { paragraph: { indent: { left: 1080, hanging: 360 } } } }] },
      { reference: "numbered-list",
        levels: [{ level: 0, format: LevelFormat.DECIMAL, text: "%1.", alignment: AlignmentType.LEFT,
          style: { paragraph: { indent: { left: 720, hanging: 360 } } } }] },
      { reference: "workflow-steps",
        levels: [{ level: 0, format: LevelFormat.DECIMAL, text: "%1.", alignment: AlignmentType.LEFT,
          style: { paragraph: { indent: { left: 720, hanging: 360 } } } }] },
      { reference: "function-list",
        levels: [{ level: 0, format: LevelFormat.DECIMAL, text: "%1.", alignment: AlignmentType.LEFT,
          style: { paragraph: { indent: { left: 720, hanging: 360 } } } }] }
    ]
  },
  sections: [{
    properties: {
      page: { margin: { top: 1440, right: 1440, bottom: 1440, left: 1440 } }
    },
    headers: {
      default: new Header({ children: [new Paragraph({ 
        alignment: AlignmentType.RIGHT,
        children: [new TextRun({ text: "brrr Package - Usage Manual", italics: true, size: 18, color: "666666" })]
      })] })
    },
    footers: {
      default: new Footer({ children: [new Paragraph({ 
        alignment: AlignmentType.CENTER,
        children: [new TextRun({ text: "Page ", size: 18 }), new TextRun({ children: [PageNumber.CURRENT], size: 18 }), new TextRun({ text: " of ", size: 18 }), new TextRun({ children: [PageNumber.TOTAL_PAGES], size: 18 })]
      })] })
    },
    children: [
      // Title Page
      new Paragraph({ heading: HeadingLevel.TITLE, children: [new TextRun("brrr Package")] }),
      new Paragraph({ 
        alignment: AlignmentType.CENTER, 
        spacing: { after: 120 },
        children: [new TextRun({ text: "Benefit-Risk Visualization for Clinical Trials", size: 28, color: "4a5568" })] 
      }),
      new Paragraph({ 
        alignment: AlignmentType.CENTER, 
        spacing: { after: 480 },
        children: [new TextRun({ text: "Usage Manual - Version 0.1.0", size: 24, italics: true, color: "718096" })] 
      }),
      
      // Introduction Section
      new Paragraph({ heading: HeadingLevel.HEADING_1, children: [new TextRun("1. Introduction")] }),
      new Paragraph({ children: [new TextRun("The "), new TextRun({ text: "brrr", bold: true }), new TextRun(" package provides comprehensive tools for creating benefit-risk visualization plots commonly used in clinical trial analysis and regulatory submissions. These visualizations present forest plots showing treatment effects with confidence intervals, allowing clear comparison of benefits and risks.")] }),
      new Paragraph({ spacing: { before: 120 }, children: [new TextRun("Key features include:")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun("Multi-axis forest plot visualizations")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun("Customizable headers with benefit direction arrows")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun("Support for both linear and logarithmic scales")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun("Flexible legends and color palettes")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun("Draft watermark capability for preliminary outputs")] }),
      
      // Installation Section
      new Paragraph({ heading: HeadingLevel.HEADING_1, children: [new TextRun("2. Installation")] }),
      new Paragraph({ heading: HeadingLevel.HEADING_2, children: [new TextRun("2.1 From GitHub")] }),
      new Paragraph({ children: [new TextRun("Install the development version from GitHub using the devtools package:")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("# install.packages(\"devtools\")")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("devtools::install_github(\"mrppdex/brrr\")")] }),
      
      new Paragraph({ heading: HeadingLevel.HEADING_2, children: [new TextRun("2.2 Dependencies")] }),
      new Paragraph({ children: [new TextRun("The package requires the following R packages:")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun({ text: "grid", bold: true }), new TextRun(" - Base R graphics system (included with R)")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun({ text: "dplyr", bold: true }), new TextRun(" (>= 1.0.0) - Data manipulation")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun({ text: "R6", bold: true }), new TextRun(" - Object-oriented programming")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun({ text: "rlang", bold: true }), new TextRun(" - Programming with tidyverse")] }),
      
      // Quick Start Section
      new Paragraph({ heading: HeadingLevel.HEADING_1, children: [new TextRun("3. Quick Start")] }),
      new Paragraph({ children: [new TextRun("Here's a minimal example to create a benefit-risk visualization:")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, spacing: { before: 120 }, children: [new TextRun("library(brrr)")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("library(grid)")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("# Load sample data")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("data(mock_data)")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("# Define column specifications")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("columns_specs <- c(")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  'Benefits' = 'endpoint',")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  'Treatment' = 'treatment',")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  'Placebo' = 'placebo',")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  'Comparison' = 'col3'")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun(")")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("# Define column widths")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("breaks_widths <- c(0.2, -0.1, 0.1, 0.2)")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("# Create the plot")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("result <- plot_br(")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  data = mock_data,")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  columns_specs = columns_specs,")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  breaks_widths = breaks_widths,")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  split_axis_by_col = 'axis_number',")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  axis_labels_col = 'estimator',")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  split_box_by_col = 'endpoint'")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun(")")] }),
      
      // Page Break before Core Functions
      new Paragraph({ children: [new PageBreak()] }),
      
      // Core Functions Section
      new Paragraph({ heading: HeadingLevel.HEADING_1, children: [new TextRun("4. Core Functions")] }),
      
      // plot_br
      new Paragraph({ heading: HeadingLevel.HEADING_2, children: [new TextRun("4.1 plot_br()")] }),
      new Paragraph({ children: [new TextRun("The main function for creating benefit-risk visualizations. Creates forest plots with customizable axes, headers, and styling.")] }),
      new Paragraph({ spacing: { before: 120 }, children: [new TextRun({ text: "Key Parameters:", bold: true })] }),
      
      new Table({
        columnWidths: [2340, 6120],
        margins: { top: 50, bottom: 50, left: 100, right: 100 },
        rows: [
          new TableRow({
            tableHeader: true,
            children: [
              new TableCell({ borders: cellBorders, width: { size: 2340, type: WidthType.DXA }, shading: { fill: "e2e8f0", type: ShadingType.CLEAR },
                children: [new Paragraph({ children: [new TextRun({ text: "Parameter", bold: true })] })] }),
              new TableCell({ borders: cellBorders, width: { size: 6120, type: WidthType.DXA }, shading: { fill: "e2e8f0", type: ShadingType.CLEAR },
                children: [new Paragraph({ children: [new TextRun({ text: "Description", bold: true })] })] })
            ]
          }),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2340, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "data", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 6120, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Data frame with value, lower, upper columns")] })] })
          ]}),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2340, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "columns_specs", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 6120, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Named vector: display names → column names")] })] })
          ]}),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2340, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "breaks_widths", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 6120, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Numeric vector of column widths")] })] })
          ]}),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2340, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "split_axis_by_col", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 6120, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Column to split plot into separate axes")] })] })
          ]}),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2340, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "axis_labels_col", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 6120, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Column containing axis labels")] })] })
          ]}),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2340, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "split_box_by_col", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 6120, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Column to split boxes within each axis")] })] })
          ]}),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2340, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "box_group", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 6120, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Previous plot_br() result to chain plots")] })] })
          ]}),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2340, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "is_draft", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 6120, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Logical; add DRAFT watermark (default TRUE)")] })] })
          ]})
        ]
      }),
      
      // page_options
      new Paragraph({ heading: HeadingLevel.HEADING_2, children: [new TextRun("4.2 page_options")] }),
      new Paragraph({ children: [new TextRun("R6 class for customizing plot appearance. Create with "), new TextRun({ text: "page_options$new()", font: "Courier New" }), new TextRun(".")] }),
      new Paragraph({ spacing: { before: 120 }, children: [new TextRun({ text: "Common Methods:", bold: true })] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun({ text: "set_palette(colors)", font: "Courier New" }), new TextRun(" - Set color palette")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun({ text: "set_label_font_size(size)", font: "Courier New" }), new TextRun(" - Adjust label font size")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun({ text: "set_header_font_size(size)", font: "Courier New" }), new TextRun(" - Adjust header font size")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun({ text: "set_page_parameter(name, value)", font: "Courier New" }), new TextRun(" - Set margin/layout parameters")] }),
      
      // add_legend
      new Paragraph({ heading: HeadingLevel.HEADING_2, children: [new TextRun("4.3 add_legend()")] }),
      new Paragraph({ children: [new TextRun("Adds a legend to the plot with customizable items.")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, spacing: { before: 120 }, children: [new TextRun("# Example legend items")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("legend_items <- list(")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  list(type='line', pch=1, col=1, label='Treatment 100mg'),")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  list(type='line', pch=2, col=2, label='Treatment 500mg')")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun(")")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("add_legend(legend_items, xpos=0.05, ypos=0.15,")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("           width=0.9, height=0.06)")] }),
      
      // Page Break before Data Requirements
      new Paragraph({ children: [new PageBreak()] }),
      
      // Data Requirements Section
      new Paragraph({ heading: HeadingLevel.HEADING_1, children: [new TextRun("5. Data Requirements")] }),
      new Paragraph({ heading: HeadingLevel.HEADING_2, children: [new TextRun("5.1 Required Columns")] }),
      new Paragraph({ children: [new TextRun("Your data frame must contain the following columns:")] }),
      
      new Table({
        columnWidths: [2000, 6460],
        margins: { top: 50, bottom: 50, left: 100, right: 100 },
        rows: [
          new TableRow({
            tableHeader: true,
            children: [
              new TableCell({ borders: cellBorders, width: { size: 2000, type: WidthType.DXA }, shading: { fill: "e2e8f0", type: ShadingType.CLEAR },
                children: [new Paragraph({ children: [new TextRun({ text: "Column", bold: true })] })] }),
              new TableCell({ borders: cellBorders, width: { size: 6460, type: WidthType.DXA }, shading: { fill: "e2e8f0", type: ShadingType.CLEAR },
                children: [new Paragraph({ children: [new TextRun({ text: "Description", bold: true })] })] })
            ]
          }),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2000, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "value", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 6460, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Point estimate (e.g., hazard ratio, odds ratio)")] })] })
          ]}),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2000, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "lower", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 6460, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Lower bound of confidence interval")] })] })
          ]}),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2000, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "upper", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 6460, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Upper bound of confidence interval")] })] })
          ]})
        ]
      }),
      
      new Paragraph({ heading: HeadingLevel.HEADING_2, children: [new TextRun("5.2 Optional Columns")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun({ text: "reversed", font: "Courier New" }), new TextRun(" - Logical; reverse axis direction")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun({ text: "logscale", font: "Courier New" }), new TextRun(" - Logical; use logarithmic scale")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun({ text: "logbase", font: "Courier New" }), new TextRun(" - Numeric; base for log scale (default 2)")] }),
      
      new Paragraph({ heading: HeadingLevel.HEADING_2, children: [new TextRun("5.3 Sample Data")] }),
      new Paragraph({ children: [new TextRun("The package includes two sample datasets:")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun({ text: "mock_data", bold: true }), new TextRun(" - Benefit/efficacy endpoints")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun({ text: "mock_data_risks", bold: true }), new TextRun(" - Risk/safety endpoints")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, spacing: { before: 120 }, children: [new TextRun("# Load sample data")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("data(mock_data)")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("head(mock_data)")] }),
      
      // Page Break before Complete Example
      new Paragraph({ children: [new PageBreak()] }),
      
      // Complete Example Section
      new Paragraph({ heading: HeadingLevel.HEADING_1, children: [new TextRun("6. Complete Example")] }),
      new Paragraph({ children: [new TextRun("This example creates a combined benefit-risk visualization with both efficacy and safety data:")] }),
      
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, spacing: { before: 120 }, children: [new TextRun("library(brrr)")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("library(grid)")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("# Load data")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("data(mock_data)")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("data(mock_data_risks)")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("# Column specifications for benefits")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("columns_specs <- c(")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  'Benefit' = 'endpoint',")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  'Treatment\\n(N=100)' = 'treatment',")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  'Placebo\\n(N=100)' = 'placebo',")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  'Comparison' = 'col3'")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun(")")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("breaks_widths <- c(0.2, -0.1, 0.1, 0.2)")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("value_collapse <- c(TRUE, FALSE, FALSE, FALSE)")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("# Create benefits section")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("part1 <- plot_br(")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  data = mock_data,")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  columns_specs = columns_specs,")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  breaks_widths = breaks_widths,")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  split_axis_by_col = 'axis_number',")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  axis_labels_col = 'estimator',")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  split_box_by_col = 'endpoint',")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  value_collapse = value_collapse,")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  is_draft = FALSE")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun(")")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("# Add risks section (chained)")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("columns_specs_risks <- c(")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  'Risk' = 'endpoint',")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  'Treatment\\n(N=100)' = 'treatment',")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  'Placebo\\n(N=100)' = 'placebo',")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  'Comparison' = 'txt_val'")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun(")")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("part2 <- plot_br(")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  data = mock_data_risks,")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  columns_specs = columns_specs_risks,")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  breaks_widths = breaks_widths,")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  split_axis_by_col = 'axis_number',")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  axis_labels_col = 'estimator',")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  split_box_by_col = 'endpoint',")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  box_group = part1,  # Chain to previous plot")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  userect = TRUE")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun(")")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("# Add legend")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("legend_items <- list(")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  list(type='line', pch=1, col=1, label='Treatment 100mg'),")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  list(type='line', pch=2, col=2, label='Treatment 500mg')")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun(")")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("opts <- part2$options")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("add_legend(")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  legend_items = legend_items,")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  xpos = unit(opts$PAGE_LEFT_MARGIN, 'npc'),")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  ypos = unit(part2$last_y - 0.1, 'npc'),")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  width = unit(opts$HEADER_WIDTH, 'npc'),")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("  height = unit(0.06, 'npc')")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun(")")] }),
      
      // Page Break before Troubleshooting
      new Paragraph({ children: [new PageBreak()] }),
      
      // Troubleshooting Section
      new Paragraph({ heading: HeadingLevel.HEADING_1, children: [new TextRun("7. Troubleshooting")] }),
      
      new Paragraph({ heading: HeadingLevel.HEADING_2, children: [new TextRun("7.1 Common Errors")] }),
      
      new Paragraph({ spacing: { before: 120 }, children: [new TextRun({ text: "\"data is missing required columns\"", bold: true, italics: true })] }),
      new Paragraph({ children: [new TextRun("Ensure your data frame contains 'value', 'lower', and 'upper' columns with numeric values.")] }),
      
      new Paragraph({ spacing: { before: 120 }, children: [new TextRun({ text: "\"breaks_widths must have the same length as columns_specs\"", bold: true, italics: true })] }),
      new Paragraph({ children: [new TextRun("The number of column widths must match the number of column specifications.")] }),
      
      new Paragraph({ spacing: { before: 120 }, children: [new TextRun({ text: "\"box_group must be a result from a previous plot_br() call\"", bold: true, italics: true })] }),
      new Paragraph({ children: [new TextRun("When chaining plots, pass the complete result object from the previous plot_br() call.")] }),
      
      new Paragraph({ heading: HeadingLevel.HEADING_2, children: [new TextRun("7.2 Tips")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun("Use "), new TextRun({ text: "is_draft = FALSE", font: "Courier New" }), new TextRun(" for final production outputs")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun("Negative values in "), new TextRun({ text: "breaks_widths", font: "Courier New" }), new TextRun(" create invisible gaps between columns")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun("Use "), new TextRun({ text: "value_collapse = TRUE", font: "Courier New" }), new TextRun(" for columns where values should be de-duplicated")] }),
      new Paragraph({ numbering: { reference: "main-bullets", level: 0 }, children: [new TextRun("For logarithmic axes, add "), new TextRun({ text: "logscale = TRUE", font: "Courier New" }), new TextRun(" column to your data")] }),
      
      // API Reference Summary
      new Paragraph({ heading: HeadingLevel.HEADING_1, children: [new TextRun("8. Function Reference Summary")] }),
      
      new Table({
        columnWidths: [2800, 5660],
        margins: { top: 50, bottom: 50, left: 100, right: 100 },
        rows: [
          new TableRow({
            tableHeader: true,
            children: [
              new TableCell({ borders: cellBorders, width: { size: 2800, type: WidthType.DXA }, shading: { fill: "e2e8f0", type: ShadingType.CLEAR },
                children: [new Paragraph({ children: [new TextRun({ text: "Function", bold: true })] })] }),
              new TableCell({ borders: cellBorders, width: { size: 5660, type: WidthType.DXA }, shading: { fill: "e2e8f0", type: ShadingType.CLEAR },
                children: [new Paragraph({ children: [new TextRun({ text: "Purpose", bold: true })] })] })
            ]
          }),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2800, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "plot_br()", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 5660, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Create complete benefit-risk visualization")] })] })
          ]}),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2800, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "page_options$new()", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 5660, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Create customization options object")] })] })
          ]}),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2800, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "create_header()", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 5660, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Create plot header (used internally)")] })] })
          ]}),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2800, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "add_box()", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 5660, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Add forest plot box (used internally)")] })] })
          ]}),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2800, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "add_legend()", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 5660, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Add legend to visualization")] })] })
          ]}),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2800, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "add_benefit_arrows()", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 5660, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Add directional arrows to header")] })] })
          ]}),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2800, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "plot_axis()", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 5660, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Create customized axis")] })] })
          ]}),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2800, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "plot_forest_tree()", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 5660, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Plot individual forest element")] })] })
          ]}),
          new TableRow({ children: [
            new TableCell({ borders: cellBorders, width: { size: 2800, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun({ text: "get_metadata()", font: "Courier New" })] })] }),
            new TableCell({ borders: cellBorders, width: { size: 5660, type: WidthType.DXA }, children: [new Paragraph({ children: [new TextRun("Extract metadata from data frame")] })] })
          ]})
        ]
      }),
      
      // Final info
      new Paragraph({ spacing: { before: 480 }, children: [new TextRun({ text: "For more information, see the package documentation:", italics: true })] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("?plot_br")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("?page_options")] }),
      new Paragraph({ style: "Code", shading: { fill: "f7fafc", type: ShadingType.CLEAR }, children: [new TextRun("?add_legend")] }),
      
      new Paragraph({ spacing: { before: 240 }, alignment: AlignmentType.CENTER, children: [new TextRun({ text: "— End of Manual —", italics: true, color: "718096" })] })
    ]
  }]
});

// Save the document
Packer.toBuffer(doc).then(buffer => {
  fs.writeFileSync("/home/claude/brrr_fixed/brrr_usage_manual.docx", buffer);
  console.log("Manual created: brrr_usage_manual.docx");
});
