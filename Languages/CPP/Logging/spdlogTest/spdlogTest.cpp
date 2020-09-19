// spdLog is a header-only logging library which is hosted at: https://github.com/gabime/spdlog

// Wiki and QuickStart is here (spdLog is vast; this demo just scratches the surface)
//		https://github.com/gabime/spdlog/wiki

// Official documentation can be found here: https://spdlog.docsforge.com/master/0.faq/

#include "..\..\..\..\Libraries\spdlog\spdlog.h"
#include "..\..\..\..\Libraries\spdlog\sinks\basic_file_sink.h"
#include "..\..\..\..\Libraries\spdlog\async.h"						// support for async logging
#include "..\..\..\..\Libraries\spdlog\sinks\rotating_file_sink.h"	// support for rotating logging

#include <iostream>

int main() 
{
	// Use the default logger (stdout, multi-threaded, colored)
	spdlog::info("Welcome to spdlog!");
	spdlog::error("Some error message with arg: {}", 1);
 
	spdlog::warn("Easy padding in numbers like {:08d}", 12);
	spdlog::info("Hello {} {} !!", "param1", 123.4);
	spdlog::critical("Support for int: {0:d};  hex: {0:x};  oct: {0:o}; bin: {0:b}", 42);
	spdlog::info("Support for floats {:03.2f}", 1.23456);
	spdlog::info("Positional args are {1} {0}.", "too", "supported");
	spdlog::info("{}{:<30}{}", "!", "left aligned", "!");
	spdlog::info("{}{:>30}{}", "!", "right aligned", "!");
 
	spdlog::set_level(spdlog::level::debug);	// Set global log level to debug
	spdlog::debug("This message should be displayed.");
 
	// Change log pattern
	spdlog::info("");
	spdlog::info("(changing the log pattern)");
	spdlog::set_pattern("[%H:%M:%S %z] [%n] [%^---%L---%$] [thread %t] %v");
	spdlog::info("Hello {} {} !!", "param1", 123.4);
	spdlog::info("{}{:<30}{}", "!", "left aligned", "!");
	spdlog::info("{}{:>30}{}", "!", "right aligned", "!");
 
	// Compile time log levels (define SPDLOG_ACTIVE_LEVEL to desired level)
	SPDLOG_TRACE("Some trace message with param {}", 42);
	SPDLOG_DEBUG("Some debug message");
 
	// Create a simple file logger (multi-threaded)
	// Note: Leave the last parameter ("truncate") as default (or false) to append to the log
	spdlog::info("");
	spdlog::info("(creating a simple file logger)");
	auto file_simple = spdlog::basic_logger_mt("basic_logger", "logs/log_simple.txt", true);

	// To change the default logger use this:
	//		spdlog::set_default_logger(file_simple);
	//		spdlog::warn("...and this will be written to {}", "file");

	// Write entries to a log file on disk (use some of the same ones we wrote to the console)
	file_simple->warn("Easy padding in numbers like {:08d}", 12);
	file_simple->info("Hello {} {} !!", "param1", 123.4);
	file_simple->info("{}{:<30}{}", "!", "left aligned", "!");
	file_simple->info("{}{:>30}{}", "!", "right aligned", "!");
	file_simple->flush();	// NB! Ensure the file contents hit disk...

	// Create an asynchronous logger
	spdlog::info("");
	spdlog::info("(creating an asyncrhonous file logger)");
	try
	{
		auto file_async = spdlog::basic_logger_mt<spdlog::async_factory>(
			"async_file_logger", "logs/log_async.txt", true);
		for (int entry = 1; entry <= 100; ++entry)
		{
			file_async->info("Async message #{}", entry);
		}
	}
	catch (const spdlog::spdlog_ex& ex)
	{
		std::cout << "Log initialization failed: " << ex.what() << std::endl;
	}

	// Create a rotating log (ie. with a maximum file size)
	// Note: Maximum size would typically be much larger eg. 1048576 * 5 (or 5MB). This is the
	//		number of bytes before the next log file is created.
	// Note: Set "max_files" = 1 to delete the file contents when the size is reached
	spdlog::info("");
	spdlog::info("(creating a rotating file logger)");
	auto file_rotating = spdlog::rotating_logger_mt(
		"file_logger", "logs/log_rotating.txt", 4096, 3, true);
	for (int line = 1; line <= 200; ++line)
	{
		file_rotating->info("Line #{}", line);
	}

	// Under Visual Studio, this must be called before main finishes to workaround a known VS issue
	spdlog::drop_all(); 
}
