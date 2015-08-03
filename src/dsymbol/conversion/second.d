/**
 * This file is part of DCD, a development tool for the D programming language.
 * Copyright (C) 2014 Brian Schott
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

module dsymbol.conversion.second;

//import dsymbol.modulecache;
//import dsymbol.scope_;
//import dsymbol.symbol;
//import dsymbol.builtin.symbols;
//import std.experimental.logger;
//
//void secondPass(Scope* currentScope, ref ModuleCache cache)
//{
//	if (currentScope.parent is null)
//	{
//		foreach (t; builtinSymbols[])
//			currentScope.addSymbol(t, false);
//	}
//
//	foreach (im; currentScope.importInformation[])
//	{
//		DSymbol* moduleSymbol = cache.getModuleSymbol(im.modulePath);
//		if (moduleSymbol is null)
//			continue;
//		if (im.importedSymbols.empty)
//		{
//			foreach (s; moduleSymbol.opSlice())
//				currentScope.addSymbol(s, false);
//		}
//		else
//		{
//			foreach (sel; im.importedSymbols[])
//			{
//				if (sel[0] is null)
//				{
//					foreach (part; moduleSymbol.getPartsByName(sel[1]))
//						currentScope.addSymbol(part, false);
//				}
//				else
//				{
//
//				}
//			}
//		}
//	}
//	foreach (child; currentScope.children[])
//		secondPass(child, cache);
//}
