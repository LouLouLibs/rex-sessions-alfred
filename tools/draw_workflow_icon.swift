// Draw the workflow's own icon (a prompt on a dark tile), so the bundle needs no Rex artwork.
//
//   swift draw_workflow_icon.swift OUTPUT_PNG
import AppKit

let size = 256
let bitmap = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: size, pixelsHigh: size,
                              bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
                              colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)!
NSGraphicsContext.saveGraphicsState()
NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: bitmap)
let tile = NSRect(x: 12, y: 12, width: size - 24, height: size - 24)
NSColor(srgbRed: 0.13, green: 0.14, blue: 0.17, alpha: 1).set()
NSBezierPath(roundedRect: tile, xRadius: 52, yRadius: 52).fill()
let text = NSAttributedString(string: ">_", attributes: [
    .font: NSFont.monospacedSystemFont(ofSize: 112, weight: .bold),
    .foregroundColor: NSColor(srgbRed: 0.45, green: 0.85, blue: 0.62, alpha: 1),
])
let textSize = text.size()
text.draw(at: NSPoint(x: tile.midX - textSize.width / 2, y: tile.midY - textSize.height / 2))
NSGraphicsContext.restoreGraphicsState()
try bitmap.representation(using: .png, properties: [:])!.write(to: URL(fileURLWithPath: CommandLine.arguments[1]))
